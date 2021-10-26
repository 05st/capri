{-# Language OverloadedStrings #-}

module Lexer where

import Data.Text (Text, singleton, pack)
import Data.Functor.Identity
import Data.Void

import qualified Control.Monad.State as S

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import OperatorDef

type Parser = ParsecT Void Text (S.State [OperatorDef])

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

charLiteral :: Parser Char
charLiteral = between (char '\'') (char '\'') L.charLiteral

stringLiteral :: Parser String
stringLiteral = char '\"' *> manyTill L.charLiteral (char '\"')

decimal :: Parser Integer
decimal = lexeme L.decimal

signedInteger :: Parser Integer
signedInteger = L.signed sc decimal

octal :: Parser Integer
octal = lexeme L.octal

hexadecimal :: Parser Integer
hexadecimal = lexeme L.hexadecimal

binary :: Parser Integer
binary = lexeme L.binary

float :: Parser Double
float = lexeme L.float

signedFloat :: Parser Double
signedFloat = L.signed sc float

identChar :: Parser Char
identChar = alphaNumChar <|> oneOf ("_'" :: [Char])

identifier :: Parser Text
identifier = lexeme (mappend <$> (singleton <$> letterChar) <*> (pack <$> many identChar))

reserved :: Text -> Parser Text
reserved kw = lexeme (string kw <* notFollowedBy identChar)

typeIdentifier :: Parser Text
typeIdentifier = lexeme (mappend <$> (singleton <$> upperChar) <*> (pack <$> many identChar))

operChar :: Parser Char
operChar = oneOf (":!@#$%^&*-+=<>./?\\|~" :: [Char])

operator :: Parser Text
operator = lexeme (pack <$> many operChar)

reservedOp :: Text -> Parser Text
reservedOp op = lexeme (string op <* notFollowedBy operChar)

parens :: Parser a -> Parser a
parens = between (char '(') (char ')') . lexeme

braces :: Parser a -> Parser a
braces = between (char '{') (char '}')

angles :: Parser a -> Parser a
angles = between (char '<') (char '>')

brackets :: Parser a -> Parser a
brackets = between (char '[') (char ']') 

semi :: Parser Char
semi = lexeme (char ';')

colon :: Parser Char
colon = lexeme (char ':')

comma :: Parser Char
comma = lexeme (char ',')

dot :: Parser Char
dot = lexeme (char '.')

{-
reservedNames =
    ["fn", "mut", "if", "else", "match", "while", "op", "sizeof", "return", "extern",
     "infixl", "infixr", "infix", "prefix", "postfix",
     "i8", "i16", "i32", "i64",
     "u8", "u16", "u32", "u64",
     "f32", "f64",
     "str", "char", "bool", "unit",
     "true", "false", "()",
     "_"]
reservedOpNames = ["=", ":=", "=>", "->", ":", "@", "+", "-", "*", "/", "==", "!=", ">", "<", ">=", "<="]
-}
