{-# Language OverloadedStrings #-}
{-# Language TupleSections #-}

module Parser (Parser.parse) where

import System.FilePath

import Data.Text (Text, pack, split)
import Data.Function
import Data.List

import Text.Megaparsec
import Text.Megaparsec.Char

import Control.Arrow
import Control.Monad.Combinators.Expr
import Control.Monad.Reader

import Lexer
import Syntax
import Type
import Name
import OperatorDef
import SyntaxInfo
import Debug.Trace (trace)

-- Operator defs (pre)
pModuleOpDefs :: Parser [OperatorDef]
pModuleOpDefs = concat <$> manyTill (try ((:[]) <$> pOperatorDef) <|> ([] <$ anySingle)) eof

-- Module
pModule :: [Text] -> Parser UntypedModule
pModule modPath = do
    synInfo <- pSyntaxInfo
    symbol "module"
    name <- identifier
    semi

    externs <- many pExtern
    imports <- many (try pImport)

    Module synInfo name modPath imports externs <$> manyTill pTopLvlDecl eof
    where
        pImport = do
            isPub <- option False (True <$ symbol "pub")
            symbol "import"
            (isPub,) <$> (sepBy1 identifier (symbol "::") <* semi)
        pExtern = do
            symbol "extern"
            name <- identifier
            paramTypes <- parens (sepBy pBaseType comma)
            symbol ":"
            retType <- pType
            semi
            return (name, paramTypes, retType)

-- Top Level Declarations
pTopLvlDecl :: Parser UntypedTopLvl
pTopLvlDecl = do
    isPub <- option False (True <$ symbol "pub")
    choice (map ($ isPub) [pFuncOperDecl, pTypeAliasDecl, pEnumDecl])

pFuncOperDecl :: Bool -> Parser UntypedTopLvl
pFuncOperDecl isPub = do
    synInfo <- pSyntaxInfo
    isOper <- (True <$ symbol "op") <|> (False <$ symbol "fn")
    name <- if isOper then oper <$> pOperatorDef else identifier
    params <- pParams
    retAnnot <- optional pTypeAnnot
    TLFunc synInfo () isPub isOper (Unqualified name) params retAnnot <$> (pExpression <* semi)

pOperatorDef :: Parser OperatorDef
pOperatorDef = do
    assoc <- pAssoc
    prec <- decimal
    OperatorDef assoc prec <$> operator
    where
        pAssoc = (ALeft <$ symbol "infixl") <|> (ARight <$ symbol "infixr") <|> (ANone <$ symbol "infix")
            <|> (APrefix <$ symbol "prefix") <|> (APostfix <$ symbol "postfix")

pTypeAliasDecl :: Bool -> Parser UntypedTopLvl
pTypeAliasDecl isPub = do
    synInfo <- pSyntaxInfo
    symbol "type"
    name <- typeIdentifier
    params <- option [] (angles (sepBy1 (TV <$> identifier) comma))
    symbol "="
    TLType synInfo isPub (Unqualified name) params <$> (pType <* semi)

pEnumDecl :: Bool -> Parser UntypedTopLvl
pEnumDecl isPub = do
    synInfo <- pSyntaxInfo
    symbol "enum"
    name <- typeIdentifier
    params <- option [] (angles (sepBy1 (TV <$> identifier) comma))
    symbol "="
    variants <- angles (sepBy1 pEnumVariant comma) <* semi
    return (TLEnum synInfo isPub (Unqualified name) params variants)
    where
        pEnumVariant = do
            label <- identifier
            typeWraps <- option [] (parens (sepBy1 pType comma))
            return (label, typeWraps)

-- Declarations
pDecl :: Parser UntypedDecl
pDecl = pVarDecl <|> (DStmt <$> pStmt)

pVarDecl :: Parser UntypedDecl
pVarDecl = do
    synInfo <- pSyntaxInfo
    symbol "let"
    isMut <- option False (True <$ symbol "mut")
    name <- identifier
    annot <- optional pTypeAnnot
    symbol "="
    DVar synInfo isMut (Unqualified name) annot <$> (pExpression <* semi)

-- Statements
pStmt :: Parser UntypedStmt
pStmt = pRetStmt <|> pWhileStmt <|> (SExpr <$> (pExpression <* semi))

pRetStmt :: Parser UntypedStmt
pRetStmt = do
    symbol "return"
    SRet <$> (pExpression <* semi)

pWhileStmt :: Parser UntypedStmt
pWhileStmt = do
    synInfo <- pSyntaxInfo
    symbol "while"
    cond <- pExpression
    SWhile synInfo cond <$> (pExpression <* semi)

-- Expressions
pExpression :: Parser UntypedExpr
pExpression = do
    opers <- ask
    synInfo <- pSyntaxInfo

    let table = mkTable synInfo opers
    makeExprParser pTerm table
    where
        mkTable synInfo = map (map (toParser synInfo)) . groupBy ((==) `on` prec) . sortBy (flip compare `on` prec)
        toParser synInfo (OperatorDef assoc _ oper) = case assoc of
            ANone -> infixOp oper (EBinOp synInfo () . Unqualified $ oper)
            ALeft -> infixlOp oper (EBinOp synInfo () . Unqualified $ oper)
            ARight -> infixrOp oper (EBinOp synInfo () . Unqualified $ oper)
            APrefix -> prefixOp oper (EUnaOp synInfo () . Unqualified $ oper)
            APostfix -> postfixOp oper (EUnaOp synInfo () . Unqualified $ oper)
        infixOp name f = InfixN (f <$ symbol name)
        infixlOp name f = InfixL (f <$ symbol name)
        infixrOp name f = InfixR (f <$ symbol name)
        prefixOp name f = Prefix (f <$ symbol name)
        postfixOp name f = Postfix (f <$ symbol name)

pTerm :: Parser UntypedExpr
pTerm = pIfExpr <|> pMatchExpr <|> try pCast <|> pAssign

pIfExpr :: Parser UntypedExpr
pIfExpr = do
    synInfo <- pSyntaxInfo
    symbol "if"
    cond <- pExpression
    onTrue <- pExpression
    onFalse <- option (ELit synInfo () LUnit) (symbol "else" *> pExpression)
    pure (EIf synInfo () cond onTrue onFalse)

pMatchExpr :: Parser UntypedExpr
pMatchExpr = do
    synInfo <- pSyntaxInfo
    symbol "match"
    mexpr <- pExpression
    EMatch synInfo () mexpr <$> braces (sepBy1 pMatchBranch comma)
    where
        pMatchBranch = do
            pat <- pPattern
            symbol "=>"
            (pat,) <$> pExpression

pCast :: Parser UntypedExpr
pCast = do
    synInfo <- pSyntaxInfo
    target <- parens pType
    ECast synInfo target <$> pCall

pAssign :: Parser UntypedExpr
pAssign = do
    synInfo <- pSyntaxInfo
    lhs <- pRecordSelect
    option lhs (do
        symbol "="
        EAssign synInfo () lhs <$> pExpression)
        {-
        rhs <- pExpression

        -- Desugar <record>.<label> = <expr>
        -- into    <record> = {<label> = <expr> | {<record> - <label>}}
        return $
            case lhs of
                ERecordSelect info _ recordExpr label ->
                    EAssign synInfo () recordExpr (ERecordExtend info () rhs label (ERecordRestrict info () recordExpr label))
                _ -> EAssign synInfo () lhs rhs)
        -}

pRecordSelect :: Parser UntypedExpr
pRecordSelect = do
    expr <- pCall
    synInfo <- pSyntaxInfo
    option expr (do
        dot
        ERecordSelect synInfo () expr <$> identifier)

pCall :: Parser UntypedExpr
pCall = do
    synInfo <- pSyntaxInfo
    expr <- pValue
    option expr (do
        args <- parens (sepBy pExpression comma)
        pure (ECall synInfo () expr args))

pValue :: Parser UntypedExpr
pValue = pClosure <|> pLiteralExpr <|> pVariant <|> try pVariable <|> try pRecordRestrict <|> try pRecord <|> pBlock <|> parens pExpression

pClosure :: Parser UntypedExpr
pClosure = do
    synInfo <- pSyntaxInfo
    closedVars <- brackets (sepBy identifier comma)
    paramsParsed <- pParams
    retAnnot <- optional pTypeAnnot
    EClosure synInfo () closedVars paramsParsed retAnnot <$> pExpression

pLiteralExpr :: Parser UntypedExpr
pLiteralExpr = do
    synInfo <- pSyntaxInfo
    ELit synInfo () <$> pLiteral

pVariable :: Parser UntypedExpr
pVariable = do
    synInfo <- pSyntaxInfo
    EVar synInfo () . Unqualified <$> (identifier <|> parens operator)

pRecord :: Parser UntypedExpr
pRecord = do
    synInfo <- pSyntaxInfo
    (items, extends) <- braces ((,) <$> sepBy recordItem comma <*> recordExtend synInfo)
    let result = foldr (.) (const extends) [ERecordExtend synInfo () label expr | (expr, label) <- items] ()
    return result
    where
        recordItem = (,) <$> identifier <*> (symbol "=" *> pExpression)
        recordExtend info = option (ERecordEmpty info ()) (symbol "|" *> pExpression)

pRecordRestrict :: Parser UntypedExpr
pRecordRestrict = do
    synInfo <- pSyntaxInfo
    (expr, label) <- braces ((,) <$> pExpression <*> (symbol "-" *> identifier))
    return (ERecordRestrict synInfo () expr label)

pVariant :: Parser UntypedExpr
pVariant = do
    synInfo <- pSyntaxInfo
    enumName <- Unqualified <$> typeIdentifier
    symbol "::"
    variantLabel <- identifier
    exprs <- option [] (parens (sepBy1 pExpression comma))
    return (EVariant synInfo () enumName variantLabel exprs)

pBlock :: Parser UntypedExpr
pBlock = braces (do
    synInfo <- pSyntaxInfo
    decls <- many (try pDecl)
    synInfo' <- pSyntaxInfo
    result <- option (ELit synInfo' () LUnit) pExpression
    pure (EBlock synInfo () decls result))

-- Literals
pLiteral :: Parser Lit
pLiteral = try (LFloat <$> signed float) <|> (LInt <$> integer)
    <|> (LChar <$> charLiteral) <|> (LString . pack <$> stringLiteral)
    <|> (LBool True <$ symbol "true") <|> (LBool False <$ symbol "false")
    <|> (LUnit <$ symbol "()")

integer :: Parser Integer
integer = try octal <|> try binary <|> try hexadecimal <|> signed decimal

-- Types
pType :: Parser Type
pType = try pArrowType <|> pRecordType <|> pTypePtr <?> "type"

pArrowType :: Parser Type
pArrowType = do
    paramTypes <- sepBy pTypeApp comma
    symbol "->"
    TArrow paramTypes <$> pType

pRecordType :: Parser Type
pRecordType = braces (option TRecordEmpty rowExtend)
    where
        rowExtend = do
            rowsParsed <- sepBy1 row comma
            let rowExtends = map (uncurry TRecordExtend) rowsParsed
            extended <- option TRecordEmpty (symbol "|" *> (((TConst . Unqualified <$> typeIdentifier) <|> pRecordType) <?> "record type or type identifier"))

            pure (foldr ($) extended rowExtends)
        row = (,) <$> identifier <*> (colon *> pType)

pTypePtr :: Parser Type
pTypePtr = do
    typ <- pTypeApp
    option typ (do
        symbol "*"
        pure (TPtr typ))

pTypeApp :: Parser Type
pTypeApp = do
    typ <- pBaseType
    option typ (do
        args <- angles (sepBy1 pType comma)
        pure (TApp typ args))

pBaseType :: Parser Type
pBaseType = pConstType <|> pVarType <|> parens pType

pVarType :: Parser Type
pVarType = TVar . TV <$> identifier

pConstType :: Parser Type
pConstType = userDefined <|> pPrimType
    where
        userDefined = TConst . Unqualified <$> typeIdentifier

pPrimType :: Parser Type
pPrimType = TConst <$> choice (map (\s -> Unqualified s <$ symbol s)
    ["i8", "i16", "i32", "i64", "u8", "u16", "u32", "u64", "f16", "f32", "f64", "char", "str", "bool", "unit"]) <?> "Primitive type"

-- Patterns
pPattern :: Parser Pattern
pPattern = parens pPattern <|> try pVariantPattern <|> pWildPattern <|> pVarPattern <|> pLitPattern

pVariantPattern :: Parser Pattern
pVariantPattern = do
    enumName <- Unqualified <$> typeIdentifier
    symbol "::"
    variantLabel <- identifier
    varNames <- option [] (parens (sepBy1 (Unqualified <$> identifier) comma))
    return (PVariant enumName variantLabel varNames) -- No nested patterns for now

pWildPattern :: Parser Pattern
pWildPattern = PWild <$ symbol "_"

pVarPattern :: Parser Pattern
pVarPattern = PVar . Unqualified <$> identifier

pLitPattern :: Parser Pattern
pLitPattern = PLit <$> pLiteral

-- Utility
pSyntaxInfo :: Parser SyntaxInfo
pSyntaxInfo = SyntaxInfo <$> getSourcePos

pParams :: Parser [(Name, Maybe Type)]
pParams = parens (sepBy ((,) . Unqualified <$> identifier <*> optional pTypeAnnot) comma)

pTypeAnnot :: Parser Type
pTypeAnnot = colon *> pType

-- Run
parse :: FilePath -> [(FilePath, Text)] -> [(FilePath, Text)] -> Either String UntypedProgram
parse rootDir files stlFiles = do
    let opdefs = concat (concat (traverse (uncurry (runParser (runReaderT pModuleOpDefs []))) (stlFiles ++ files)))

    -- Yeah I know this code is quite a monstrosity
    let (filePaths, contents) = unzip files
    let parseFiles = map (\(filePath, content) -> runParser (runReaderT (pModule (getModPath rootDir filePath)) opdefs) filePath content) files
    let parseStl = map (\(filePath, content) -> runParser (runReaderT (pModule (getModPath "" filePath)) opdefs) filePath content) stlFiles

    left errorBundlePretty (sequence (parseStl ++ parseFiles))
    where
        getModPath dir path = map pack ((init . (\fp -> splitDirectories fp \\ splitDirectories dir)) path)