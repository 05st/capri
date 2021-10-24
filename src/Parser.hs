{-# Language OverloadedStrings #-}
{-# Language LambdaCase #-}
{-# Language TupleSections #-}

module Parser (Parser.parse) where

import Data.List
import Data.Function
import Data.Functor.Identity
import qualified Data.Text as T

import Text.Parsec
import Text.Parsec.Expr

import Lexer
import Syntax
import OperatorDef
import qualified Text.Parsec.Prim as Parsec

type Parser a = ParsecT T.Text [OperatorDef] Identity a

-- Declarations
declaration :: Parser UntypedDecl
declaration = externDecl <|> funcDecl <|> operDecl <|> try varDecl <|> (DStmt <$> statement)

externDecl :: Parser UntypedDecl
externDecl = do
    reserved "extern"
    fn <- identifier
    paramTypes <- parens (sepBy type' comma)
    retType <- typeAnnot
    semi
    return (DExtern fn paramTypes retType)

funcDecl :: Parser UntypedDecl
funcDecl = do
    reserved "fn"
    name <- identifier
    paramsParsed <- params
    retAnnot <- optionMaybe typeAnnot
    reservedOp "=>"
    expr <- expression
    semi
    return $ DFunc () name paramsParsed retAnnot expr

operDecl :: Parser UntypedDecl
operDecl = do
    reserved "op"
    assocParsed <- assoc
    precedence <- decimal
    whitespace
    oper <- operator
    paramsParsed <- params
    retAnnot <- optionMaybe typeAnnot
    reservedOp "=>"
    expr <- expression
    semi
    let opdef = OperatorDef assocParsed precedence oper
    modifyState (opdef :)
    return $ DOper () opdef oper paramsParsed retAnnot expr
    where
        assoc = (ALeft <$ reserved "infixl") <|> (ARight <$ reserved "infixr") <|> (ANone <$ reserved "infix")
            <|> (APrefix <$ reserved "prefix") <|> (APostfix <$ reserved "postfix")

varDecl :: Parser UntypedDecl
varDecl = do
    isMut <- option False (True <$ reserved "mut")
    name <- identifier
    annot <- optionMaybe typeAnnot
    reservedOp ":="
    DVar () isMut name annot <$> (expression <* semi)

-- Statements
statement :: Parser UntypedStmt
statement = retStmt <|> whileStmt <|> (SExpr <$> (expression <* semi))

retStmt :: Parser UntypedStmt
retStmt = do
    reserved "return"
    SRet <$> (expression <* semi)

whileStmt :: Parser UntypedStmt
whileStmt = do
    reserved "while"
    cond <- expression
    SWhile cond <$> (expression <* semi)

-- Expressions
expression :: Parser UntypedExpr
expression = do
    opers <- getState 
    let table = mkTable opers
    buildExpressionParser table term
    where
        mkTable = map (map toParser) . groupBy ((==) `on` prec) . sortBy (flip compare `on` prec)
        toParser (OperatorDef assoc _ oper) = case assoc of
            ALeft -> infixOp oper (EBinOp () oper) (toAssoc assoc)
            ARight -> infixOp oper (EBinOp () oper) (toAssoc assoc)
            ANone -> infixOp oper (EBinOp () oper) (toAssoc assoc)
            APrefix -> prefixOp oper (EUnaOp () oper)
            APostfix -> postfixOp oper (EUnaOp () oper)
        infixOp name f = Infix (reservedOp (T.unpack name) >> return f)
        prefixOp name f = Prefix (reservedOp (T.unpack name) >> return f)
        postfixOp name f = Postfix (reservedOp (T.unpack name) >> return f)
        toAssoc ALeft = AssocLeft
        toAssoc ARight = AssocRight
        toAssoc ANone = AssocNone
        toAssoc _ = undefined

term :: Parser UntypedExpr
term = ifExpr <|> matchExpr <|> closure <|> try assign <|> (deref <|> ref <|> value)

ifExpr :: Parser UntypedExpr
ifExpr = do
    reserved "if"
    cond <- expression
    trueBody <- expression
    falseBody <- option (ELit () LUnit) (reserved "else" *> expression)
    return $ EIf () cond trueBody falseBody

matchExpr :: Parser UntypedExpr
matchExpr = do
    reserved "match"
    mexpr <- expression
    EMatch () mexpr <$> braces (sepBy1 matchBranch comma)
    where
        matchBranch = do
            pat <- pattern
            whitespace
            reservedOp "->"
            (pat,) <$> expression

closure :: Parser UntypedExpr
closure = do
    closedVars <- brackets (sepBy identifier comma)
    paramsParsed <- params
    retAnnot <- optionMaybe typeAnnot
    reservedOp "=>"
    EClosure () closedVars paramsParsed retAnnot <$> expression

assign :: Parser UntypedExpr
assign = do
    var <- deref <|> value
    reservedOp "="
    EAssign () var <$> expression

deref :: Parser UntypedExpr
deref = do
    whitespace *> char '*' <* whitespace
    EDeref () <$> value

ref :: Parser UntypedExpr
ref = do
    whitespace *> char '&' <* whitespace
    ERef () <$> value

value :: Parser UntypedExpr
value = (try sizeof <|> try cast <|> try call) <|> (ELit () <$> literal <* whitespace) <|> try variable <|> block <|> parens expression

sizeof :: Parser UntypedExpr 
sizeof = do
    reserved "sizeof"
    ESizeof () <$> type'

cast :: Parser UntypedExpr
cast = do
    typ <- type'
    arg <- parens expression
    return $ ECast () typ arg

call :: Parser UntypedExpr
call = do
    id <- identifier -- TODO
    args <- parens (sepBy expression comma)
    return $ ECall () (EVar () id) args

variable :: Parser UntypedExpr
variable = EVar () <$> (identifier <|> parens operator)

block :: Parser UntypedExpr
block = braces $ do
    decls <- many (try declaration)
    result <- option (ELit () LUnit) expression
    whitespace
    return $ EBlock () decls result

-- Literals
literal :: Parser Lit
literal = try (LFloat <$> float) <|> (LInt <$> integer) 
    <|> (LChar <$> charLiteral) <|> (LString <$> stringLiteral)
    <|> (LBool True <$ reserved "true") <|> (LBool False <$ reserved "false")
    <|> (LUnit <$ reserved "()")

integer :: Parser Integer
integer = intLit <|> try octal <|> try hexadecimal

-- Parse types
type' :: Parser Type
type' = try typeFunc <|> typeBase

typeFunc :: Parser Type
typeFunc = do
    inputTypes <- sepBy typeBase comma
    reservedOp "->"
    TFunc inputTypes <$> type'

typeBase :: Parser Type
typeBase = do
    t <- typePrim <|> (TCon <$> typeIdentifier) <|> {-(TVar . TV <$> identifier) <|> -} parens type'
    option t (TPtr t <$ (whitespace *> char '*' <* whitespace))

typePrim :: Parser Type
typePrim = choice $ map (\s -> TCon s <$ reserved (T.unpack s))
    ["i8", "i16", "i32", "i64", "u8", "u16", "u32", "u64", "f16", "f32", "f64", "str", "char", "bool", "unit"]

-- Parse patterns
pattern :: Parser Pattern
pattern = parens pattern <|> patternWild <|> try patternAs <|> patternVar <|> patternLit

patternWild :: Parser Pattern
patternWild = PWild <$ reserved "_"

patternVar :: Parser Pattern
patternVar = PVar <$> identifier

patternAs :: Parser Pattern
patternAs = do
    var <- identifier
    reservedOp "@"
    PAs var <$> pattern

patternLit :: Parser Pattern
patternLit = PLit <$> literal

-- Other
typeAnnot :: Parser Type
typeAnnot = reservedOp ":" *> type'

params :: Parser [(T.Text, Maybe Type)]
params = parens (sepBy ((,) <$> identifier <*> optionMaybe typeAnnot) comma)

-- Run parser
parse :: T.Text -> Either ParseError [UntypedDecl]
parse = runParser (many1 declaration) builtinOpers "juno"

builtinOpers :: [OperatorDef]
builtinOpers =
    [OperatorDef ALeft 5 "+", OperatorDef ALeft 5 "-",
     OperatorDef ALeft 10 "*", OperatorDef ALeft 10 "/",
     OperatorDef ALeft 3 "==", OperatorDef ALeft 3 "!=",
     OperatorDef ALeft 4 ">", OperatorDef ALeft 4 "<",
     OperatorDef ALeft 4 ">=", OperatorDef ALeft 4 "<=",
     OperatorDef ALeft 1 "||", OperatorDef ALeft 2 "&&"]
