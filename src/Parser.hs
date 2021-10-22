module Parser where

import Data.Functor.Identity

import Text.Parsec
import Text.Parsec.Expr

import Syntax
import OperatorDef

type Parser a = ParsecT String [OperatorDef] Identity a

