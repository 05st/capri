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

type Parser = ParsecT Void Text (S.State ([OperatorDef], [Text]))

reservedNames :: [Text]
reservedNames =
    ["fn", "mut", "if", "else", "match", "while", "op", "sizeof", "return", "extern", "type", "struct", "module", "import", "pub",
     "infixl", "infixr", "infix", "prefix", "postfix",
     "i8", "i16", "i32", "i64",
     "u8", "u16", "u32", "u64",
     "f32", "f64",
     "char", "bool", "unit",
     "true", "false", "()",
     "_"]

reservedOpNames :: [Text]
reservedOpNames = ["=", ":=", "=>", "->", ":", "@", "+", "-", "*", "/", "%", "==", "!=", ">", "<", ">=", "<=", "::"]

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

charLiteral :: Parser Char
charLiteral = lexeme (between (char '\'') (char '\'') L.charLiteral)

stringLiteral :: Parser String
stringLiteral = lexeme (char '\"' *> manyTill L.charLiteral (char '\"'))

decimal :: Parser Integer
decimal = lexeme L.decimal

signedInteger :: Parser Integer
signedInteger = lexeme (L.signed sc decimal)

octal :: Parser Integer
octal = lexeme (char '0' *> char 'o' *> L.octal)

hexadecimal :: Parser Integer
hexadecimal = lexeme (char '0' *> char 'x' *> L.hexadecimal)

binary :: Parser Integer
binary = lexeme (char '0' *> char 'b' *> L.binary)

float :: Parser Double
float = lexeme L.float

signedFloat :: Parser Double
signedFloat = lexeme (L.signed sc float)

identChar :: Parser Char
identChar = alphaNumChar-- <|> oneOf ("_'" :: [Char])

identifier :: Parser Text
identifier = (lexeme . try) (p >>= check)
  where
    p = mappend <$> (singleton <$> letterChar) <*> (pack <$> many identChar)
    check x =
        if x `elem` reservedNames
            then fail $ "keyword " ++ show x ++ " cannot be an identifier"
            else return x

reserved :: Text -> Parser ()
reserved kw = lexeme (string kw *> notFollowedBy identChar)

typeIdentifier :: Parser Text
typeIdentifier = lexeme (mappend <$> (singleton <$> upperChar) <*> (pack <$> many identChar))

operChar :: Parser Char
operChar = oneOf (":!@#$%^&*-+=<>./?\\|~" :: [Char])

operator :: Parser Text
operator = lexeme (p >>= check)
  where
    p = pack <$> many operChar
    check x =
        if x `elem` reservedOpNames
            then fail $ "operator " ++ show x ++ " is reserved"
            else return x

reservedOp :: Text -> Parser Text
reservedOp op = (lexeme . try) (string op <* notFollowedBy operChar)

parens :: Parser a -> Parser a
parens = between (lexeme $ char '(') (lexeme $ char ')')

braces :: Parser a -> Parser a
braces = between (lexeme $ char '{') (lexeme $ char '}')

angles :: Parser a -> Parser a
angles = between (lexeme $ char '<') (lexeme $ char '>')

brackets :: Parser a -> Parser a
brackets = between (lexeme $ char '[') (lexeme $ char ']') 

semi :: Parser Char
semi = lexeme (char ';')

colon :: Parser Char
colon = lexeme (char ':')

comma :: Parser Char
comma = lexeme (char ',')

dot :: Parser Char
dot = lexeme (char '.')
