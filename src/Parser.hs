{-# Language TupleSections #-}

module Parser where

import Data.Functor.Identity

import Text.Parsec
import Text.Parsec.Expr

import Lexer
import Syntax
import OperatorDef

type Parser a = ParsecT String [OperatorDef] Identity a

declaration :: Parser UntypedDecl
declaration = funcDecl <|> operDecl <|> varDecl <|> (DStmt <$> statement)

funcDecl :: Parser UntypedDecl
funcDecl = do
    reserved "fn"
    name <- identifier
    paramsParsed <- params
    retAnnot <- optionMaybe typeAnnot
    reservedOp "=>"
    expr <- expression
    semi
    pure $ DFunc name paramsParsed retAnnot expr

operDecl :: Parser UntypedDecl
operDecl = do
    reserved "op"
    assocParsed <- assoc
    precedence <- decimal
    oper <- operator
    paramsParsed <- params
    retAnnot <- optionMaybe typeAnnot
    reservedOp "=>"
    expr <- expression
    semi
    let opdef = OperatorDef assocParsed precedence oper
    pure $ DOper opdef oper paramsParsed retAnnot expr
    where
        assoc = (ALeft <$ reserved "infixl") <|> (ARight <$ reserved "infixr") <|> (ANone <$ reserved "infix")
            <|> (APrefix <$ reserved "prefix") <|> (APostfix <$ reserved "postfix")

varDecl :: Parser UntypedDecl
varDecl = do
    isMut <- option False (True <$ reserved "mut")
    name <- identifier
    annot <- optionMaybe typeAnnot
    reservedOp ":="
    DVar isMut name annot <$> (expression <* semi)

statement :: Parser UntypedStmt
statement = SExpr <$> (expression <* semi)

expression :: Parser UntypedExpr
expression = undefined

literal :: Parser Lit
literal = undefined

type' :: Parser Type
type' = try typeFunc <|> typeBase

typeFunc :: Parser Type
typeFunc = do
    inputTypes <- sepBy typeBase comma
    reservedOp "->"
    TFunc inputTypes <$> type'

typeBase :: Parser Type
typeBase = typePrim <|> (TCon <$> typeIdentifier) <|> (TVar <$> identifier) <|> parens type'

typePrim :: Parser Type
typePrim = choice $ map (\s -> TCon s <$ reserved s)
    ["i8", "i16", "i32", "i64", "u8", "u16", "u32", "u64", "f32", "f64", "str", "bool", "unit"]

typeAnnot :: Parser Type
typeAnnot = reservedOp ":" *> type'

params :: Parser [(String, Maybe Type)]
params = parens (sepBy ((,) <$> identifier <*> optionMaybe typeAnnot) comma)
