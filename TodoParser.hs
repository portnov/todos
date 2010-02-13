{-# LANGUAGE UnicodeSyntax, NoMonomorphismRestriction, TypeSynonymInstances, DeriveDataTypeable #-}
module TodoParser
--     (TodoItem (..),
--      Todo, TodoMap, flattern,
--      consTodoMap,
--      loadTodo,
--     )
    where

-- import Prelude hiding (putStrLn,readFile,getContents)
-- import System.IO.UTF8
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
import Dates

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

pItem ∷ Int → Parser TodoItem
pItem year = do
    pos ← getPosition
    s ← pSpaces
    stat ← pWord
    dates ← (try (pSpecDates year) <|> return [])
    tags ← (try pTags <|> return [])
    namew ← many1 pWord
    pSpaces
    deps ← (try pDeps <|> return [])
    pSpaces
    descr ← many (noneOf "\n\r")
    pSpaces
    many ⋄ oneOf "\n\r"
    return ⋄ Item {
        itemLevel = fromIntegral $ length s,
        itemName = unwords namew,
        itemTags = tags,
        depends = deps,
        itemStatus = stat,
        itemDescr = descr,
        startDate = lookup StartDate dates,
        endDate = lookup EndDate dates,
        deadline = lookup Deadline dates,
        fileName = sourceName pos,
        lineNr = sourceLine pos }

pWord ∷ Parser String
pWord = do
    w ← many1 (noneOf " \t\n\r")
    (try pSpace') <|> (return w)
    return w

pItems ∷ Int → Parser [TodoItem]
pItems year = do
  its ← many (pItem year)
  eof
  return its

unwords' lst =
  let (hd:tl) = map (filter (/='\r')) lst
  in  case tl of
        [] -> hd
        _  -> hd ++ "    {" ++ (unwords tl) ++ "}"

filterN n prefix lst = 
  let zipped = zip [0..] lst
      good   = filter (isGood . snd) zipped
      lns    = map fst good
      sub k l = (take l) . (drop k)
      ans = map unwords' [sub j n lst | j <- lns]
      isGood x = prefix `isPrefixOf` x
      cut = drop (1+length prefix) 
  in (map (+1) lns, map cut ans)

filterJoin ∷ Int -> String → String → ([Int], String)
filterJoin n prefix str = 
  let (ns, lns) = filterN n prefix (lines str)
  in  (ns, unlines lns)

parsePlain ∷ Int → SourceName → String → [TodoItem]
parsePlain year path text = 
  case parse (pItems year) path text of
      Right items → items
      Left e → error ⋄ show e

parseAlternate ∷ Int -> String → Int → SourceName → String → [TodoItem]
parseAlternate next prefix year path text = 
  let (ns, filtered) = filterJoin next prefix text
      renumber lst = zipWith renumber1 ns lst
      renumber1 n item = item {lineNr=n}
  in case parse (pItems year) path filtered of
       Right items → renumber items
       Left e      → error $ show e
