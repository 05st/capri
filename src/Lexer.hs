{-# Language OverloadedStrings #-}

module Lexer where

import Data.Char
import Data.Text (Text, singleton)
import Data.Void

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import OperatorDef

type Parser = Parsec Void Text

reservedNames :: [Text]
reservedNames =
    ["module", "import",
     "pub", "fn", "op", "type", "extern",
     "infixl", "infixr", "infix", "prefix", "postfix",
     "let", "mut",
     "return",
     "if", "else", "match", "while",
     "sizeof",
     "i8", "i16", "i32", "i64",
     "u8", "u16", "u32", "u64",
     "f32", "f64",
     "char", "bool", "unit",
     "true", "false", "()",
     "_"]

reservedOpers :: [Text]
reservedOpers =
    ["=", "=>", "->", ":", "@", "::"]

spaces :: Parser ()
spaces = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaces

symbol :: Text -> Parser Text
symbol = L.symbol spaces

charLiteral :: Parser Char
charLiteral = lexeme (between (char '\'') (char '\'') L.charLiteral)

stringLiteral :: Parser String
stringLiteral = lexeme (char '\"' *> manyTill L.charLiteral (char '\"'))

decimal :: Parser Integer
decimal = lexeme L.decimal

octal :: Parser Integer
octal = lexeme (char '0' *> char 'o' *> L.octal)

hexadecimal :: Parser Integer
hexadecimal = lexeme (char '0' *> char 'x' *> L.hexadecimal)

binary :: Parser Integer
binary = lexeme (char '0' *> char 'b' *> L.binary)

float :: Parser Double
float = lexeme L.float

signed :: Parser Integer -> Parser Integer
signed p = lexeme (L.signed spaces p)

identPred :: Char -> Bool
identPred c = isAlphaNum c || c `elem` ("_'" :: String)

identifier :: Parser Text
identifier = lexeme (p >>= check)
    where
        p = mappend <$> (singleton <$> letterChar) <*> takeWhileP (Just "identifier character") identPred
        check x =
            if x `elem` reservedNames
                then fail ("keyword " ++ show x ++ " cannot be an identifier")
                else return x

typeIdentifier :: Parser Text
typeIdentifier = lexeme (mappend <$> (singleton <$> upperChar) <*> takeWhileP (Just "identifier character") identPred)

operPred :: Char -> Bool
operPred c = c `elem` (":!@#$%^&*-+=<>./?\\|~" :: String)

operator :: Parser Text
operator = lexeme (p >>= check)
    where
        p = takeWhile1P (Just "operator character") operPred
        check x =
            if x `elem` reservedOpers
                then fail ("operator " ++ show x ++ " is reserved")
                else return x

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
