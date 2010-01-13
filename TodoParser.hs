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

pSpaces ∷ Parser String
pSpaces = many pSpace

pDeps ∷ Parser [String]
pDeps = do
    string "("
    ws ← (many1 ⋄ noneOf ",)\n\r") `sepBy` (char ',')
    string ")"
    return $ map strip ws

pTags ∷ Parser [String]
pTags = do
    ts ← between (char '[') (char ']') $ word `sepBy1` pSpace
    pSpaces
    return ts
  where
    word = many1 (noneOf " \t\n\r]")

pItem ∷ Parser TodoItem
pItem = do
    pos ← getPosition
    s ← pSpaces
    stat ← pWord
    tags ← (try pTags <|> return [])
    namew ← many1 pWord
    pSpaces
    deps ← (try pDeps <|> return [])
    pSpaces
    descr ← many (noneOf "\n\r")
    pSpaces
    many ⋄ oneOf "\n\r"
    return ⋄ Item (fromIntegral $ length s) (unwords namew) tags deps stat descr (sourceName pos) (sourceLine pos)

pWord ∷ Parser String
pWord = do
    w ← many1 (noneOf " \t\n\r")
    (try pSpace') <|> (return w)
    return w

pItems ∷  Parser [TodoItem]
pItems = do
  its ← many pItem
  eof
  return its

filterN ∷ String → [String] → ([Int], [String])
filterN prefix lst = unzip $ filterN' 1 lst
  where
    filterN' k [] = []
    filterN' k (x:xs) | prefix `isPrefixOf` x = (k, cut x):filterN' (k+1) xs
                      | otherwise             = filterN' (k+1) xs
    cut = drop (1+length prefix)

filterJoin ∷ String → String → ([Int], String)
filterJoin prefix str = 
  let (ns, lns) = filterN prefix (lines str)
  in  (ns, unlines lns)

parsePlain ∷ SourceName → String → [TodoItem]
parsePlain path text = 
  case parse pItems path text of
      Right items → items
      Left e → error ⋄ show e

parseAlternate ∷ String → SourceName → String → [TodoItem]
parseAlternate prefix path text = 
  let (ns, filtered) = filterJoin prefix text
      renumber lst = zipWith renumber1 ns lst
      renumber1 n item = item {lineNr=n}
  in case parse pItems path filtered of
       Right items → renumber items
       Left e      → error $ show e
