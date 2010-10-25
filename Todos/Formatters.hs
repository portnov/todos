{-# LANGUAGE UnicodeSyntax, TypeSynonymInstances #-}
module Todos.Formatters where

import Control.Monad
import Control.Monad.Reader
import System.Console.ANSI

import Todos.Unicode
import Todos.Types
import Todos.Config
import Todos.Color

type Transformer = Reader Config (Todo → [Todo])

data OutItem = OutString String
             | OutSetColor ColorIntensity Color
             | SetBold
             | ResetAll
    deriving (Show)

type Formatter = Reader Config [OutItem]

newtype IOList = IOL [Formatter]

startFormat ∷ Formatter
startFormat = return []

outString ∷ String → Formatter
outString s = return [OutString s]

newLine ∷ Formatter
newLine = outString "\n"

class ConfigAdd a where
  (<++>) ∷ Formatter → a → Formatter

instance ConfigAdd Formatter where
  (<++>) = liftM2 (⧺)

instance ConfigAdd String where
  cm <++> s = cm <++> ((return [OutString s]) ∷ Formatter)

setBold ∷  IO ()
setBold = setSGR [SetConsoleIntensity BoldIntensity]

setColor ∷ ColorIntensity → Color → IO ()
setColor int clr = setSGR [SetColor Foreground int clr]

reset ∷  IO ()
reset = setSGR []

outItem ∷  OutItem → IO ()
outItem (OutString s)   = putStr s
outItem (OutSetColor i c) = setColor i c
outItem SetBold         = setBold
outItem ResetAll        = reset

runFormatter ∷ Config → Formatter → IO ()
runFormatter conf cm = 
  let lst = runReader cm conf
  in  mapM_ outItem lst

class ConfigShow s where
  configShow ∷ s → Formatter

instance ConfigShow String where
  configShow s = return [OutString s]

instance ConfigShow Formatter where
  configShow = id
  
showIO ∷ (ConfigShow a) ⇒ Config → a → IO ()
showIO conf = (runFormatter conf) ∘ configShow

bold ∷ String → Formatter
bold s = do
  col ← asks outColors 
  if col
    then return [SetBold, OutString s, ResetAll]
    else return [OutString s]

colorStatus ∷ String → Formatter
colorStatus st = do
  let (int, clr) = statusColor st
  col ← asks outColors 
  if col
    then return [OutSetColor int clr, OutString st, ResetAll]
    else return [OutString st]

instance ConfigShow TodoItem where
    configShow item = startFormat <++> colorStatus s <++> " " <++> dates <++> tags <++> bold name <++> (if null descr then "" else "    "⧺descr)
      where
        n = itemLevel item
        name = itemName item
        ts = itemTags item
        s = itemStatus item
        descr = itemDescr item
        dates | null dates' = ""
              | otherwise = "(" ⧺ dates' ⧺ ") "
        dates' = showDates [StartDate `is` startDate item, EndDate `is` endDate item, Deadline `is` deadline item]
        tags = if null ts
                 then ""
                 else "[" ⧺ (unwords ts) ⧺ "] "

