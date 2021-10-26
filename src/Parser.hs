{-# Language OverloadedStrings #-}
{-# Language LambdaCase #-}
{-# Language TupleSections #-}

module Parser (Parser.parse) where

import Data.Text (Text, pack)
import Data.Void
import Data.List
import Data.Function
import Data.Functor.Identity

import Text.Megaparsec
import Text.Megaparsec.Char

import Control.Monad.Combinators.Expr
import qualified Control.Monad.State as S

import Lexer
import Syntax
import Type
import OperatorDef

-- Module
parseModule :: Parser UntypedModule
parseModule = sc *> many topLvlDecl <* sc

-- Top Level Declarations
topLvlDecl :: Parser UntypedTopLvl
topLvlDecl = topLvlFuncDecl <|> topLvlOperDecl <|> topLvlExternDecl

topLvlFuncDecl :: Parser UntypedTopLvl
topLvlFuncDecl = do
    reserved "fn"
    name <- identifier
    paramsParsed <- params
    retAnnot <- optional typeAnnot
    expr <- expression
    semi
    return $ TLFunc () name paramsParsed retAnnot expr

topLvlOperDecl :: Parser UntypedTopLvl
topLvlOperDecl = do
    reserved "op"
    assocParsed <- assoc
    precedence <- decimal
    oper <- operator
    paramsParsed <- params
    retAnnot <- optional typeAnnot
    expr <- expression
    semi

    let opdef = OperatorDef assocParsed precedence oper
    S.modify (opdef :)

    return $ TLOper () opdef oper paramsParsed retAnnot expr
    where
        assoc = (ALeft <$ reserved "infixl") <|> (ARight <$ reserved "infixr") <|> (ANone <$ reserved "infix")
            <|> (APrefix <$ reserved "prefix") <|> (APostfix <$ reserved "postfix")

topLvlExternDecl :: Parser UntypedTopLvl
topLvlExternDecl = do
    reserved "extern"
    fn <- identifier
    paramTypes <- parens (sepBy type' comma)
    retType <- typeAnnot
    semi
    return (TLExtern fn paramTypes retType)

-- Declarations
declaration :: Parser UntypedDecl
declaration = try varDecl <|> (DStmt <$> statement)

varDecl :: Parser UntypedDecl
varDecl = do
    isMut <- option False (True <$ reserved "mut")
    name <- identifier
    annot <- optional (try typeAnnot)
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
    body <- expression
    semi
    return (SWhile cond body)

-- Expressions
expression :: Parser UntypedExpr
expression = do
    opers <- S.get
    let table = mkTable opers
    makeExprParser term table
    where
        mkTable = map (map toParser) . groupBy ((==) `on` prec) . sortBy (flip compare `on` prec)
        toParser (OperatorDef assoc _ oper) = case assoc of
            ANone -> infixOp oper (EBinOp () oper)
            ALeft -> infixlOp oper (EBinOp () oper)
            ARight -> infixrOp oper (EBinOp () oper)
            APrefix -> prefixOp oper (EUnaOp () oper)
            APostfix -> postfixOp oper (EUnaOp () oper)
        infixOp name f = InfixN (reservedOp name >> return f)
        infixlOp name f = InfixL (reservedOp name >> return f)
        infixrOp name f = InfixR (reservedOp name >> return f)
        prefixOp name f = Prefix (reservedOp name >> return f)
        postfixOp name f = Postfix (reservedOp name >> return f)

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
            reservedOp "->"
            (pat,) <$> expression

closure :: Parser UntypedExpr
closure = do
    closedVars <- brackets (sepBy identifier comma)
    paramsParsed <- params
    retAnnot <- optional typeAnnot
    reservedOp "=>"
    EClosure () closedVars paramsParsed retAnnot <$> expression

assign :: Parser UntypedExpr
assign = do
    var <- deref <|> value 
    reservedOp "="
    EAssign () var <$> expression

deref :: Parser UntypedExpr
deref = do
    symbol "*"
    EDeref () <$> value

ref :: Parser UntypedExpr
ref = do
    symbol "&"
    ERef () <$> value

value :: Parser UntypedExpr
value = (try sizeof <|> try cast <|> try call) <|> (ELit () <$> literal) <|> try variable <|> try block <|> parens expression

sizeof :: Parser UntypedExpr 
sizeof = do
    reserved "sizeof"
    arg <- try (Left <$> type') <|> (Right <$> expression)
    return (ESizeof () arg)

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
    decls <- many declaration
    result <- option (ELit () LUnit) expression
    return $ EBlock () decls result

-- Literals
literal :: Parser Lit
literal = try (LFloat <$> signedFloat) <|> (LInt <$> integer)
    <|> (LChar <$> charLiteral) <|> (LString . pack <$> stringLiteral)
    <|> (LBool True <$ reserved "true") <|> (LBool False <$ reserved "false")
    <|> (LUnit <$ reserved "()")

integer :: Parser Integer
integer = try octal <|> try binary <|> try hexadecimal <|> signedInteger

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
    t <- typePrim <|> (TCon <$> typeIdentifier) <|> parens type'
    option t (TPtr t <$ symbol "*")

typePrim :: Parser Type
typePrim = choice $ map (\s -> TCon s <$ reserved s)
    ["i8", "i16", "i32", "i64", "u8", "u16", "u32", "u64", "f16", "f32", "f64", "str", "char", "bool", "unit"]

-- Parse patterns
pattern :: Parser Pattern
pattern = parens pattern <|> patternWild <|> patternVar <|> patternLit

patternWild :: Parser Pattern
patternWild = PWild <$ reserved "_"

patternVar :: Parser Pattern
patternVar = PVar <$> identifier

patternLit :: Parser Pattern
patternLit = PLit <$> literal

-- Other
typeAnnot :: Parser Type
typeAnnot = symbol ":" *> type'

params :: Parser [(Text, Maybe Type)]
params = parens (sepBy ((,) <$> identifier <*> optional typeAnnot) comma)

-- Run parser
parse :: String -> Text -> Either String UntypedModule
parse fileName input =
    case fst $ S.runState (runParserT parseModule fileName input) builtinOpers of
        Left err -> Left (errorBundlePretty err)
        Right mod -> Right mod

builtinOpers :: [OperatorDef]
builtinOpers =
    [OperatorDef ALeft 5 "+", OperatorDef ALeft 5 "-",
     OperatorDef ALeft 10 "*", OperatorDef ALeft 10 "/",
     OperatorDef ALeft 3 "==", OperatorDef ALeft 3 "!=",
     OperatorDef ALeft 4 ">", OperatorDef ALeft 4 "<",
     OperatorDef ALeft 4 ">=", OperatorDef ALeft 4 "<=",
     OperatorDef ALeft 1 "||", OperatorDef ALeft 2 "&&"]
