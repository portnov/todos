{-# LANGUAGE UnicodeSyntax, NoMonomorphismRestriction, TypeSynonymInstances, DeriveDataTypeable, FlexibleContexts #-}
module Todos.Parser
    (parsePlain, parseAlternate)
    where

import Prelude hiding (putStrLn,readFile,getContents,print)
import Prelude.Unicode
import Data.List
import Text.Parsec
import Data.Char
import Data.Dates
import Text.Regex.PCRE

import Todos.Types
import Todos.Dates
import Todos.Config

type TParser a = Parsec String BaseConfig a

strip ∷  String → String
strip = reverse ∘ p ∘ reverse ∘ p
  where
    p = dropWhile isSpace

pSpace ∷ TParser Char
pSpace = oneOf " \t"

pSpace' ∷ TParser String
pSpace' = do
    pSpace
    return " "

pSpaces ∷ TParser String
pSpaces = many pSpace

pDeps ∷ TParser [String]
pDeps = do
    string "("
    ws ← (many1 $ noneOf ",)\n\r") `sepBy` (char ',')
    string ")"
    return $ map strip ws

pTags ∷ TParser [String]
pTags = do
    ts ← between (char '[') (char ']') $ word `sepBy1` pSpace
    pSpaces
    return ts
  where
    word = many1 (noneOf " \t\n\r]")

pItem ∷ String → DateTime → TParser TodoItem
pItem prefix date = do
    pr ← if null prefix
           then return ""
           else do
                w ← many1 (noneOf " \t\n\r")
                if w =~ prefix
                  then return w
                  else fail $ "Internal error: invalid prefix: " ⧺ w ⧺ " =~ " ⧺ prefix
    pos ← getPosition
    s ← pSpaces
    conf ← getState
    stat ← if skipStatus conf
            then case forcedStatus conf of
                    Just fs → return fs
                    Nothing → return "*"
            else do 
                rs ← pWord
                case forcedStatus conf of
                  Just fs → return fs
                  Nothing → return rs
    dates ← (try (pSpecDates date) <|> return [])
    tags ← (try pTags <|> return [])
    namew ← many1 pWord
    pSpaces
    deps ← (try pDeps <|> return [])
    pSpaces
    descr ← many (noneOf "\n\r")
    pSpaces
    many $ oneOf "\n\r"
    return $ Item {
        itemLevel = fromIntegral $ length s,
        itemPrefix = pr,
        itemName = unwords namew,
        itemTags = tags,
        depends = deps,
        itemStatus = stat,
        itemDescr = descr,
        startDate = lookup StartDate dates,
        endDate = lookup EndDate dates,
        deadline = lookup Deadline dates,
        fileName = sourceName pos,
        lineNr = sourceLine pos,
        itemNumber = 0}

pWord ∷ TParser String
pWord = do
    w ← many1 (noneOf " \t\n\r")
    (try pSpace') <|> (return w)
    return w

pItems ∷ String → DateTime → TParser [TodoItem]
pItems prefix date = do
  its ← many (pItem prefix date)
  eof
  return its

unwords' ∷  String → [String] → String
unwords' prefix lst =
  let (hd:tl) = map (filter (/='\r')) lst
      addLines = filter (not ∘ (prefix `isPrefixOf`)) tl
  in  case addLines of
        [] → hd
        _  → hd ⧺ "    {" ⧺ unwords addLines ⧺ "}"

filterN ∷ (Num a, Enum a) ⇒ Int → String → [String] → ([a], [String])
filterN n prefix lst = 
  let zipped = zip [0..] lst
      good   = filter (isGood ∘ snd) zipped
      lns    = map fst good
      sub k l = (take l) ∘ (drop k)
      ans = map (unwords' prefix) [sub j n lst | j ← lns]
      regex = makeRE prefix
      isGood x = x =~ regex
  in (map (+1) lns, ans)

makeRE ∷ String → String
makeRE x = "^(" ⧺ x ⧺ ")"

filterJoin ∷ Int → String → String → ([Int], String)
filterJoin n prefix str = 
  let (ns, lns) = filterN n prefix (lines str)
  in  (ns, unlines lns)

-- | Read list of TODO items from plain format 
parsePlain ∷ BaseConfig
           → DateTime   -- ^ Current date/time
           → SourceName -- ^ Source file name
           → String     -- ^ String to parse
           → [TodoItem]
parsePlain conf date path text = 
  case runParser (pItems "" date) conf path text of
      Right items → items
      Left e → error $ show e

-- | Read list of TODO items from alternate format
parseAlternate ∷ BaseConfig 
               → Int        -- ^ Number of lines after matching to include to item's description
               → String     -- ^ Prefix to match
               → DateTime   -- ^ Current date/time
               → SourceName -- ^ Source file name
               → String     -- ^ String to parse
               → [TodoItem]
parseAlternate conf next prefix date path text = 
  let (ns, filtered) = filterJoin next prefix text
      renumber lst = zipWith renumber1 ns lst
      renumber1 n item = item {lineNr=n}
  in case runParser (pItems (makeRE prefix) date) conf path filtered of
       Right items → renumber items
       Left e      → error $ show e

