{-# Language OverloadedStrings #-}

module Parser where

import Data.Text (Text)
import Data.Function
import Data.List

import Text.Megaparsec
import Text.Megaparsec.Char

import Control.Monad.Combinators.Expr
import Control.Monad.Reader

import Lexer
import Syntax
import Type
import Name
import OperatorDef
import SyntaxInfo

-- Operator Pre-defs (at top)
pOperatorDefs :: Parser [OperatorDef]
pOperatorDefs = manyTill pOperatorDef (symbol "module")
    where
        pOperatorDef = do
            assoc <- pAssoc
            prec <- decimal
            OperatorDef assoc prec <$> (operator <* semi)
        pAssoc = (ALeft <$ symbol "infixl") <|> (ARight <$ symbol "infixr") <|> (ANone <$ symbol "infix")
            <|> (APrefix <$ symbol "prefix") <|> (APostfix <$ symbol "synInfotfix")

-- Module
pModule :: Parser UntypedModule
pModule = do
    symbol "module"
    identifier
    Module <$> manyTill pTopLvlDecl eof

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
pTerm = pIfExpr

pIfExpr :: Parser UntypedExpr
pIfExpr = do
    synInfo <- pSyntaxInfo
    symbol "if"
    cond <- pExpression
    onTrue <- pExpression
    onFalse <- option (ELit synInfo () LUnit) (symbol "else" *> pExpression)
    pure (EIf synInfo () cond onTrue onFalse)

pValue :: Parser UntypedExpr
pValue = pLiteralExpr

pLiteralExpr :: Parser UntypedExpr
pLiteralExpr = do
    synInfo <- pSyntaxInfo
    ELit synInfo () <$> pLiteral

-- Literals
pLiteral :: Parser Lit
pLiteral = LInt <$> decimal

-- Types
pType :: Parser Type
pType = undefined

-- Utility
pSyntaxInfo :: Parser SyntaxInfo
pSyntaxInfo = SyntaxInfo <$> getSourcePos

pParams :: Parser [(Text, Maybe Type)]
pParams = parens (sepBy ((,) <$> identifier <*> optional pTypeAnnot) comma)

pTypeAnnot :: Parser Type
pTypeAnnot = colon *> pType
