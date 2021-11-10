{-# Language OverloadedStrings #-}

module Parser where

import Data.Text (Text)

import Text.Megaparsec
import Text.Megaparsec.Char

import Control.Monad.Combinators.Expr

import Lexer
import Syntax
import Type
import Name
import OperatorDef


