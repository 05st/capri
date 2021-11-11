{-# Language OverloadedStrings #-}
{-# Language TupleSections #-}

module Parser where

import Data.Text (Text)
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

-- Module
pModule :: Parser UntypedModule
pModule = do
    opdefs <- pOperatorDefs
    symbol "module"
    name <- identifier
    semi
    Module name <$> local (const opdefs) (manyTill pTopLvlDecl eof)

-- Operator Pre-defs (at top)
pOperatorDefs :: Parser [OperatorDef]
pOperatorDefs = many pOperatorDef
    where
        pOperatorDef = do
            assoc <- pAssoc
            prec <- decimal
            OperatorDef assoc prec <$> (operator <* semi)
        pAssoc = (ALeft <$ symbol "infixl") <|> (ARight <$ symbol "infixr") <|> (ANone <$ symbol "infix")
            <|> (APrefix <$ symbol "prefix") <|> (APostfix <$ symbol "postfix")

-- Top Level Declarations
pTopLvlDecl :: Parser UntypedTopLvl
pTopLvlDecl = pFuncOperDecl

pFuncOperDecl :: Parser UntypedTopLvl
pFuncOperDecl = do
    synInfo <- pSyntaxInfo
    isOper <- (True <$ symbol "op") <|> (False <$ symbol "fn")
    name <- if isOper then operator else identifier
    params <- pParams
    retAnnot <- optional pTypeAnnot

    body <- pExpression
    semi

    pure (TLFunc synInfo isOper (Unqualified name) params retAnnot body)

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
    DVar synInfo isMut name annot <$> (pExpression <* semi)

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
    lhs <- pCall
    option lhs (do
        symbol "="
        EAssign synInfo () lhs <$> pExpression)

pCall :: Parser UntypedExpr
pCall = do
    synInfo <- pSyntaxInfo
    expr <- pValue
    option expr (do
        args <- parens (sepBy pExpression comma)
        pure (ECall synInfo () expr args))

pValue :: Parser UntypedExpr
pValue = pClosure <|> pLiteralExpr <|> try pVariable <|> parens pExpression

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
    EVar synInfo () [] . Unqualified <$> (identifier <|> parens operator)

pBlock :: Parser UntypedExpr
pBlock = do
    synInfo <- pSyntaxInfo
    decls <- many (try pDecl)
    synInfo' <- pSyntaxInfo
    result <- option (ELit synInfo' () LUnit) pExpression
    pure (EBlock synInfo () decls result)

-- Literals
pLiteral :: Parser Lit
pLiteral = LInt <$> decimal

-- Types
pType :: Parser Type
pType = pArrowType <|> pBaseType

pArrowType :: Parser Type
pArrowType = do
    paramTypes <- sepBy pBaseType comma
    symbol "->"
    TArrow paramTypes <$> pType

pTypeApp :: Parser Type
pTypeApp = do
    typ <- pBaseType
    args <- angles (sepBy1 pType comma)
    pure (TApp typ args)

pBaseType :: Parser Type
pBaseType = pConstType <|> parens pType

pConstType :: Parser Type
pConstType = TConst <$> (userDefined <|> primitive)
    where
        userDefined = Unqualified <$> typeIdentifier
        primitive = choice $ map (\s -> Unqualified s <$ symbol s)
            ["i8", "i16", "i32", "i64", "u8", "u16", "u32", "u64", "f16", "f32", "f64", "char", "bool", "unit"]

-- Patterns
pPattern :: Parser Pattern
pPattern = parens pPattern <|> pVariantPattern <|> pWildPattern <|> pVarPattern <|> pLitPattern

pVariantPattern :: Parser Pattern
pVariantPattern = PVariant <$> identifier <*> pPattern

pWildPattern :: Parser Pattern
pWildPattern = PWild <$ symbol "_"

pVarPattern :: Parser Pattern
pVarPattern = PVar <$> identifier

pLitPattern :: Parser Pattern
pLitPattern = PLit <$> pLiteral

-- Utility
pSyntaxInfo :: Parser SyntaxInfo
pSyntaxInfo = SyntaxInfo <$> getSourcePos

pParams :: Parser [(Text, Maybe Type)]
pParams = parens (sepBy ((,) <$> identifier <*> optional pTypeAnnot) comma)

pTypeAnnot :: Parser Type
pTypeAnnot = colon *> pType

-- Run
parse :: Text -> Either String UntypedModule
parse input = left errorBundlePretty (runParser (runReaderT pModule []) "capri" input)
