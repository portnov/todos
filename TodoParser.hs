{-# LANGUAGE UnicodeSyntax, NoMonomorphismRestriction, TypeSynonymInstances, DeriveDataTypeable #-}
module TodoParser
--     (TodoItem (..),
--      Todo, TodoMap, flattern,
--      consTodoMap,
--      loadTodo, showTodo
--     )
    where

import Prelude hiding (putStrLn,readFile,getContents)
import System.IO.UTF8
import Control.Monad
import Data.List
import Text.ParserCombinators.Parsec
import qualified Data.Map as M
import Data.Tree
import Data.Maybe
import Data.Char
import Data.Function

import Unicode
import Types

strip = reverse ∘ p ∘ reverse ∘ p
  where
    p = dropWhile isSpace

pSpace ∷ Parser Char
pSpace = oneOf " \t"

pSpace' ∷ Parser String
pSpace' = do
    pSpace
    return " "

pDeps ∷ Parser [String]
pDeps = do
    string "("
    ws ← (many1 ⋄ noneOf ",)\n") `sepBy` (char ',')
    string ")"
    return $ map strip ws

pTags ∷ Parser [String]
pTags = do
    ts ← between (char '[') (char ']') $ word `sepBy1` pSpace
    many pSpace
    return ts
  where
    word = many1 (noneOf " \t\n]")

pItem ∷ Parser TodoItem
pItem = do
    pos ← getPosition
    s ← many pSpace
    stat ← pWord
    tags ← (try pTags <|> return [])
    namew ← many1 pWord
    many pSpace
    deps ← (try pDeps <|> return [])
    many pSpace
    descr ← many (noneOf "\n")
    many pSpace
    many ⋄ char '\n'
    return ⋄ Item (fromIntegral $ length s) (unwords namew) tags deps stat descr (sourceName pos) (sourceLine pos)

pWord ∷ Parser String
pWord = do
    w ← many1 (noneOf " \t\n")
    (try pSpace') <|> (return w)
    return w

pItems ∷  GenParser Char () [TodoItem]
pItems = do
  its ← many (try pItem)
  eof
  return its
