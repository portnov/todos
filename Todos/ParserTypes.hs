{-# LANGUAGE UnicodeSyntax #-}
module Todos.ParserTypes where

import Text.ParserCombinators.Parsec

import Todos.Config


type TParser a = GenParser Char BaseConfig a

