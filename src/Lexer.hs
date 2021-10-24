{-# Language OverloadedStrings #-}

module Lexer where

import qualified Data.Text as T
import Data.Functor.Identity

import Text.Parsec
import qualified Text.Parsec.Token as Token

tokenDef = Token.LanguageDef
    { Token.commentStart = "/*"
    , Token.commentEnd = "*/"
    , Token.commentLine = "//"
    , Token.nestedComments = True
    , Token.identStart = letter
    , Token.identLetter = alphaNum <|> oneOf "_'"
    , Token.opStart = oneOf ":!@#$%^&*-+=<>./?\\|~"
    , Token.opLetter = oneOf ":!@#$%^&*-+=<>./?\\|~"
    , Token.reservedNames =
        ["fn", "mut", "if", "else", "match", "while", "op", "sizeof", "return",
         "infixl", "infixr", "infix", "prefix", "postfix",
         "i8", "i16", "i32", "i64",
         "u8", "u16", "u32", "u64",
         "f16", "f32", "f64",
         "str", "char", "bool", "unit",
         "true", "false", "()",
         "_"]
    , Token.reservedOpNames = ["=", ":=", "=>", "->", ":", "@", "+"]
    , Token.caseSensitive = True
    }

lexer :: Token.GenTokenParser T.Text s Identity
lexer = Token.makeTokenParser tokenDef

typeLexer :: Token.GenTokenParser T.Text s Identity
typeLexer = Token.makeTokenParser $ tokenDef
    { Token.identStart = upper }

identifier = T.pack <$> Token.identifier lexer
typeIdentifier = T.pack <$> Token.identifier typeLexer
reserved = Token.reserved lexer
reservedOp = Token.reservedOp lexer
operator = T.pack <$> Token.operator lexer
parens = Token.parens lexer
decimal = Token.decimal lexer
octal = Token.octal lexer
hexadecimal = Token.hexadecimal lexer
float = Token.float lexer
semi = Token.semi lexer
colon = Token.colon lexer
whitespace = Token.whiteSpace lexer
braces = Token.braces lexer
comma = Token.comma lexer
dot = Token.dot lexer
angles = Token.angles lexer
brackets = Token.brackets lexer
charLiteral = Token.charLiteral lexer
stringLiteral = T.pack <$> Token.stringLiteral lexer
