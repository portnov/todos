{-# LANGUAGE UnicodeSyntax, TypeSynonymInstances #-}
module Todos.Formatters 
  (OutItem (..),
   Formatter,
   outItem,
   startFormat, newLine,
   ConfigShow (..),
   ConfigAdd (..)
  ) where

import Control.Monad
import Control.Monad.Reader
import System.Console.ANSI

import Todos.Unicode
import Todos.Types
import Todos.Config
import Todos.Color

-- | Item which could be printed to the console
data OutItem = OutString String
             | OutSetColor ColorIntensity Color
             | SetBold
             | ResetAll
    deriving (Show)

-- | Produce a list of OutItem's depending on PrintConfig
type Formatter = Reader PrintConfig [OutItem]

-- | Empty Formatter
startFormat ∷ Formatter
startFormat = return []

-- | Output given string
outString ∷ String → Formatter
outString s = return [OutString s]

-- | Output new line
newLine ∷ Formatter
newLine = outString "\n"

class ConfigAdd a where
  -- | Execute Formatter and a consequently
  (<++>) ∷ Formatter → a → Formatter

instance ConfigAdd Formatter where
  (<++>) = liftM2 (⧺)

instance ConfigAdd String where
  cm <++> s = cm <++> ((return [OutString s]) ∷ Formatter)

setBold ∷  IO ()
setBold = setSGR [SetConsoleIntensity BoldIntensity]

setColor ∷ ColorIntensity → Color → IO ()
setColor int clr = setSGR [SetColor Foreground int clr]

-- | Reset all (color, bold, ...) attributes
reset ∷  IO ()
reset = setSGR []

-- | Print OutItem to console
outItem ∷  OutItem → IO ()
outItem (OutString s)   = putStr s
outItem (OutSetColor i c) = setColor i c
outItem SetBold         = setBold
outItem ResetAll        = reset

-- | Similar to Show, but output can depend on PrintConfig
class ConfigShow s where
  configShow ∷ s → Formatter

instance ConfigShow String where
  configShow s = return [OutString s]

instance ConfigShow Formatter where
  configShow = id
  
-- | Output bold (and maybe colored) item name
bold ∷ TodoItem → Formatter
bold item = do
  let s = itemName item
  showColors ← asks (outColors ∘ printConfig)
  hlOn ← asks (outHighlight ∘ printConfig)
  getclr ← asks printItemColor
  hlPred ← asks doHighlight
  (hlInt, hlClr) ← asks printHighlightColor
  return $ if showColors
              then if hlOn && hlPred item
                     then [SetBold, OutSetColor hlInt hlClr, OutString s, ResetAll]
                     else case getclr item of
                           Nothing        → [SetBold, OutString s, ResetAll]
                           Just (int,clr) → [SetBold, OutSetColor int clr, OutString s, ResetAll]
              else [OutString s]

-- | Output colored item status
colorStatus ∷ String → Formatter
colorStatus st = do
  getclr ← asks printStatusColor
  let (int, clr) = getclr st
  col ← asks (outColors ∘ printConfig)
  if col
    then return [OutSetColor int clr, OutString st, ResetAll]
    else return [OutString st]

instance ConfigShow TodoItem where
    configShow item = startFormat <++> colorStatus s <++> " " <++> dates <++> tags <++> bold item <++> (if null descr then "" else "    "⧺descr)
      where
        n = itemLevel item
        ts = itemTags item
        s = itemStatus item
        descr = itemDescr item
        dates | null dates' = ""
              | otherwise = "(" ⧺ dates' ⧺ ") "
        dates' = showDates [StartDate `is` startDate item, EndDate `is` endDate item, Deadline `is` deadline item]
        tags = if null ts
                 then ""
                 else "[" ⧺ (unwords ts) ⧺ "] "

