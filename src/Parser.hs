{-# Language OverloadedStrings #-}
{-# Language LambdaCase #-}
{-# Language TupleSections #-}

module Parser (Parser.parse) where

import Data.Text (Text, pack, singleton)
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
import Name

-- Parse operator defs
parseModuleOpDefs :: Parser ()
parseModuleOpDefs = do
    manyTill (try parseModuleOpDef <|> (() <$ anySingle)) eof
    return ()
    where
        parseModuleOpDef = do
            reserved "op"
            assocParsed <- opAssoc
            precedence <- decimal
            oper <- operator

            let opdef = OperatorDef assocParsed precedence oper
            (opdefs, pubs) <- S.get
            S.put (opdef : opdefs, pubs)

opAssoc :: Parser Assoc
opAssoc = (ALeft <$ reserved "infixl") <|> (ARight <$ reserved "infixr") <|> (ANone <$ reserved "infix")
    <|> (APrefix <$ reserved "prefix") <|> (APostfix <$ reserved "postfix")

-- Module
parseModule :: Parser UntypedModule
parseModule = do
    (opdefs, _) <- S.get
    S.put (opdefs, [])

    sc
    pos <- getSourcePos
    name <- reserved "module" *> identifier <* semi
    imports <- many (reserved "import" *> parseImport <* semi)
    decls <- manyTill topLvlDecl eof

    pubs <- S.gets snd
    return (Module pos [name] imports decls pubs)
    where
        parseImport = sepBy1 identifier (reservedOp "::")

-- Top Level Declarations
topLvlDecl :: Parser UntypedTopLvl
topLvlDecl = maybePubTopLvlDecl <|> topLvlExternDecl
    where
        maybePubTopLvlDecl = do
            isPub <- option False (True <$ reserved "pub")
            choice (map ($ isPub) [topLvlFuncDecl, topLvlOperDecl, topLvlTypeDecl, topLvlStructDecl])

topLvlFuncDecl :: Bool -> Parser UntypedTopLvl
topLvlFuncDecl isPub = do
    pos <- getSourcePos
    reserved "fn"
    name <- identifier
    paramsParsed <- params
    retAnnot <- optional typeAnnot
    expr <- expression
    semi
    S.when isPub (addPub name)
    return $ TLFunc () pos (Unqualified name) paramsParsed retAnnot expr

topLvlOperDecl :: Bool -> Parser UntypedTopLvl
topLvlOperDecl isPub = do
    pos <- getSourcePos
    reserved "op"
    opAssoc
    decimal
    oper <- operator
    paramsParsed <- params
    retAnnot <- optional typeAnnot

    expr <- expression
    semi

    S.when isPub (addPub oper)
    return $ TLOper () pos (Unqualified oper) paramsParsed retAnnot expr

topLvlTypeDecl :: Bool -> Parser UntypedTopLvl
topLvlTypeDecl isPub = do
    pos <- getSourcePos
    reserved "type"
    typeName <- typeIdentifier
    typeParams <- option [] (angles (sepBy (TV <$> identifier) comma))
    reservedOp "="
    valueCons <- sepBy1 valueCon (symbol "|")
    semi

    S.when isPub (mapM_ (addPub . extractName . fst) valueCons *> addPub typeName)
    return (TLType pos (Unqualified typeName) typeParams valueCons)
    where
        valueCon = do
            conName <- typeIdentifier
            types <- parens (sepBy type' comma)
            return (Unqualified conName, types)

topLvlStructDecl :: Bool -> Parser UntypedTopLvl
topLvlStructDecl isPub = do
    pos <- getSourcePos
    reserved "struct"
    structName <- typeIdentifier
    typeParams <- option [] (angles (sepBy (TV <$> identifier) comma))
    fields <- braces (sepBy1 field comma)
    semi

    S.when isPub (addPub structName)
    return (TLStruct pos (Unqualified structName) typeParams fields)
    where
        field = (,) <$> identifier <*> typeAnnot

topLvlExternDecl :: Parser UntypedTopLvl
topLvlExternDecl = do
    reserved "extern"
    fn <- identifier
    paramTypes <- parens (sepBy type' comma)
    retType <- typeAnnot
    semi
    return (TLExtern fn paramTypes retType)

addPub :: Text -> Parser ()
addPub name = do
    (opdefs, pubs) <- S.get
    S.put (opdefs, name : pubs)

-- Declarations
declaration :: Parser UntypedDecl
declaration = try varDecl <|> (DStmt <$> statement)

varDecl :: Parser UntypedDecl
varDecl = do
    pos <- getSourcePos 
    isMut <- option False (True <$ reserved "mut")
    name <- identifier
    annot <- optional (try typeAnnot)
    reservedOp ":="
    DVar () pos isMut name annot <$> (expression <* semi)

-- Statements
statement :: Parser UntypedStmt
statement = retStmt <|> whileStmt <|> (SExpr <$> (expression <* semi))

retStmt :: Parser UntypedStmt
retStmt = do
    reserved "return"
    SRet <$> (expression <* semi)

whileStmt :: Parser UntypedStmt
whileStmt = do
    pos <- getSourcePos
    reserved "while"
    cond <- expression
    body <- expression
    semi
    return (SWhile pos cond body)

-- Expressions
expression :: Parser UntypedExpr
expression = do
    opers <- S.gets fst
    pos <- getSourcePos
    let table = mkTable pos opers
    makeExprParser term table
    where
        mkTable pos = map (map (toParser pos)) . groupBy ((==) `on` prec) . sortBy (flip compare `on` prec)
        toParser pos (OperatorDef assoc _ oper) = case assoc of
            ANone -> infixOp oper (EBinOp () pos . Unqualified $ oper)
            ALeft -> infixlOp oper (EBinOp () pos . Unqualified $ oper)
            ARight -> infixrOp oper (EBinOp () pos . Unqualified $ oper)
            APrefix -> prefixOp oper (EUnaOp () pos . Unqualified $ oper)
            APostfix -> postfixOp oper (EUnaOp () pos . Unqualified $ oper)
        infixOp name f = InfixN (reservedOp name >> return f)
        infixlOp name f = InfixL (reservedOp name >> return f)
        infixrOp name f = InfixR (reservedOp name >> return f)
        prefixOp name f = Prefix (reservedOp name >> return f)
        postfixOp name f = Postfix (reservedOp name >> return f)

term :: Parser UntypedExpr
term = ifExpr <|> matchExpr <|> try closure <|> try assign <|> try access <|> try index <|> try cast <|> try arrowIndex <|> deref <|> ref <|> value

ifExpr :: Parser UntypedExpr
ifExpr = do
    pos <- getSourcePos
    reserved "if"
    cond <- expression
    trueBody <- expression
    pos2 <- getSourcePos
    falseBody <- option (ELit () pos2 LUnit) (reserved "else" *> expression)
    return $ EIf () pos cond trueBody falseBody

matchExpr :: Parser UntypedExpr
matchExpr = do
    pos <- getSourcePos
    reserved "match"
    mexpr <- expression
    EMatch () pos mexpr <$> braces (sepBy1 matchBranch comma)
    where
        matchBranch = do
            pat <- pattern
            reservedOp "=>"
            (pat,) <$> expression

closure :: Parser UntypedExpr
closure = do
    pos <- getSourcePos
    closedVars <- brackets (sepBy identifier comma)
    paramsParsed <- params
    retAnnot <- optional typeAnnot
    EClosure () pos closedVars paramsParsed retAnnot <$> expression

assign :: Parser UntypedExpr
assign = do
    pos <- getSourcePos
    val <- deref <|> try arrowIndex <|> try index <|> try access <|> value 
    try (regularAssign pos val) <|> operatorAssign pos val
    where
        regularAssign pos val = do
            reservedOp "="
            EAssign () pos val <$> expression
        operatorAssign pos val = do -- TODO possibly make these their own AST nodes
            pos <- getSourcePos
            sc
            op <- choice (map char ['+', '-', '*', '/', '%'])
            char '='
            sc
            EAssign () pos val . EBinOp () pos (Unqualified $ singleton op) val <$> expression

access :: Parser UntypedExpr
access = do
    pos <- getSourcePos 
    expr <- value
    reservedOp "."
    EAccess () pos expr <$> identifier

index :: Parser UntypedExpr
index = do
    pos <- getSourcePos
    expr <- value
    idx <- brackets signedInteger -- if negative, let analyzer error
    return (EIndex () pos expr (fromIntegral idx))

arrowIndex :: Parser UntypedExpr
arrowIndex = do
    pos <- getSourcePos
    expr <- value
    reservedOp "->"
    EArrow () pos expr <$> identifier

deref :: Parser UntypedExpr
deref = do
    pos <- getSourcePos
    symbol "*"
    EDeref () pos <$> value

ref :: Parser UntypedExpr
ref = do
    pos <- getSourcePos
    symbol "&"
    ERef () pos <$> value

value :: Parser UntypedExpr
value = do
    pos <- getSourcePos 
    array <|> (try sizeof <|> try call <|> try structCreate) <|> (ELit () pos <$> literal) <|> try variable <|> try block <|> parens expression

sizeof :: Parser UntypedExpr 
sizeof = do
    pos <- getSourcePos 
    reserved "sizeof"
    arg <- try (Left <$> type') <|> (Right <$> expression)
    return (ESizeof () pos arg)

cast :: Parser UntypedExpr
cast = do
    pos <- getSourcePos 
    typ <- parens type'
    ECast () pos typ <$> value

call :: Parser UntypedExpr
call = do
    pos <- getSourcePos
    id <- identifier -- TODO
    args <- parens (sepBy expression comma)
    return $ ECall () pos (EVar () pos [] . Unqualified $ id) args

structCreate :: Parser UntypedExpr
structCreate = do
    pos <- getSourcePos
    structName <- typeIdentifier
    fields <- braces (sepBy1 ((,) <$> identifier <*> (reservedOp "=" *> expression)) comma)
    return (EStruct () pos (Unqualified structName) fields)

variable :: Parser UntypedExpr
variable = do
    pos <- getSourcePos
    EVar () pos [] . Unqualified <$> (identifier <|> parens operator)

block :: Parser UntypedExpr
block = braces $ do
    pos <- getSourcePos
    decls <- many (try declaration)
    pos2 <- getSourcePos
    result <- option (ELit () pos2 LUnit) expression
    return $ EBlock () pos decls result

array :: Parser UntypedExpr
array = do
    pos <- getSourcePos 
    parsed <- brackets (try manyInitArray <|> regArrayLit)
    return (parsed pos)
    where
        regArrayLit = do
            exprs <- sepBy expression comma
            return (\pos -> EArray () pos exprs)
        manyInitArray = do
            initExpr <- expression
            semi
            count <- decimal
            return (\pos -> EArray () pos (replicate (fromIntegral count) initExpr))

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
type' = typeArray <|> try typeCon <|> try typeFunc <|> typeBase

typeFunc :: Parser Type
typeFunc = do
    inputTypes <- sepBy typeCon comma
    reservedOp "->"
    TFunc inputTypes <$> type'

typeArray :: Parser Type
typeArray = do
    len <- brackets decimal
    t <- try typeCon <|> typeBase
    let typ = TArray t (fromIntegral len)
    option typ (TPtr typ <$ symbol "*")

typeCon :: Parser Type
typeCon = do
    con <- Unqualified <$> typeIdentifier
    params <- option [] (angles (sepBy type' comma))
    let t = TCon con params
    option t (TPtr t <$ symbol "*")

typeBase :: Parser Type
typeBase = do
    t <- typePrim <|> (TVar . TV <$> identifier) <|> parens type'
    option t (TPtr t <$ symbol "*")

typePrim :: Parser Type
typePrim = choice $ map (\s -> TCon (Unqualified s) [] <$ reserved s)
    ["i8", "i16", "i32", "i64", "u8", "u16", "u32", "u64", "f16", "f32", "f64", "char", "bool", "unit"]

-- Parse patterns
pattern :: Parser Pattern
pattern = parens pattern <|> patternCon <|> patternWild <|> patternVar <|> patternLit

patternCon :: Parser Pattern
patternCon = do
    conName <- typeIdentifier
    binds <- option [] (parens (sepBy (identifier <|> symbol "_") comma))
    return (PCon (Unqualified conName) binds)

patternWild :: Parser Pattern
patternWild = PWild <$ reserved "_"

patternVar :: Parser Pattern
patternVar = PVar <$> identifier

patternLit :: Parser Pattern
patternLit = PLit <$> literal

-- Other
typeAnnot :: Parser Type
typeAnnot = colon *> type'

params :: Parser [(Text, Maybe Type)]
params = parens (sepBy ((,) <$> identifier <*> optional typeAnnot) comma)

-- Run parser
parse :: [(String, Text)] -> Either String UntypedProgram
parse files =
    case sequence (fst $ S.runState (parseProgram files) (builtinOpers, [])) of
        Left err -> Left (errorBundlePretty err)
        Right prog -> Right prog
    where
        parseProgram files = do
            mapM_ (uncurry (runParserT parseModuleOpDefs)) files
            traverse (uncurry (runParserT parseModule)) files

builtinOpers :: [OperatorDef]
builtinOpers =
    [OperatorDef APrefix 10 "-",
     OperatorDef ALeft 10 "*", OperatorDef ALeft 10 "/", OperatorDef ALeft 10 "%",
     OperatorDef ALeft 5 "+", OperatorDef ALeft 5 "-",
     OperatorDef ALeft 3 "==", OperatorDef ALeft 3 "!=",
     OperatorDef ALeft 4 ">", OperatorDef ALeft 4 "<",
     OperatorDef ALeft 4 ">=", OperatorDef ALeft 4 "<=",
     OperatorDef ALeft 1 "||", OperatorDef ALeft 2 "&&"]
