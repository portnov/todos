{-# LANGUAGE UnicodeSyntax, TypeSynonymInstances, MultiParamTypeClasses, FlexibleInstances, ScopedTypeVariables #-}
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
import Todos.ConfigInstances ()

-- | Item which could be printed to the console
data OutItem = OutString String
             | OutSetColor ColorIntensity Color
             | SetBold
             | ResetAll
    deriving (Show)

-- | Produce a list of OutItem's depending on PrintConfig
type Formatter c = Reader (PrintConfig c) [OutItem]

-- | Empty Formatter
startFormat ∷ Formatter c
startFormat = return []

-- | Output given string
outString ∷ String → Formatter c
outString s = return [OutString s]

-- | Output new line
newLine ∷ Formatter c
newLine = outString "\n"

class ConfigAdd c a where
  -- | Execute Formatter and a consequently
  (<++>) ∷ Formatter c → a → Formatter c

instance ConfigAdd c (Formatter c) where
  (<++>) = liftM2 (⧺)

instance ConfigAdd c String where
  cm <++> s = cm <++> ((return [OutString s]) ∷ Formatter c)

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
class ConfigShow c s where
  configShow ∷ s → Formatter c

instance ConfigShow c String where
  configShow s = return [OutString s]

instance ConfigShow c (Formatter c) where
  configShow = id
  
-- | Output bold (and maybe colored) item name
bold ∷ (RuntimeConfig c) ⇒ TodoItem → Formatter c
bold item = do
  let s = itemName item
  showColors ← askBase outColors
  hlOn ← askBase outHighlight
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
colorStatus ∷ (RuntimeConfig c) ⇒ String → Formatter c
colorStatus st = do
  getclr ← asks printStatusColor
  let (int, clr) = getclr st
  col ← askBase outColors
  if col
    then return [OutSetColor int clr, OutString st, ResetAll]
    else return [OutString st]

printM ∷ (RuntimeConfig c) ⇒ TodoItem → Formatter c
printM item = askBase outputFormat >>= printf
  where
    printf :: (RuntimeConfig c) ⇒ String → Formatter c
    printf ""         = return []
    printf [c]        = return [OutString [c]]
    printf ('%':c:xs) = liftM2 (⧺) (itemPart c) (printf xs)
    printf (x:xs)     = do r ← printf xs
                           return $ (OutString [x]):r

    tags = filter (not ∘ null) $ itemTags item
    string s = return [OutString s]

    itemPart ∷ (RuntimeConfig c) ⇒ Char → Formatter c
    itemPart 'L' = string (show $ itemLevel item)
    itemPart 'n' = bold item 
    itemPart 't' = if null tags
                     then return []
                     else string ("[" ⧺ unwords tags ⧺ "] ")
    itemPart 's' = colorStatus (itemStatus item)
    itemPart 'd' = string (itemDescr item)
    itemPart 'f' = string (fileName item)
    itemPart 'l' = string (show $ lineNr item)
    itemPart 'D' | null dates' = return []
                 | otherwise = string $ "(" ⧺ dates' ⧺ ") "
    itemPart c   = string [c]

    dates' = showDates [StartDate `is` startDate item, EndDate `is` endDate item, Deadline `is` deadline item]

instance (RuntimeConfig c) ⇒ ConfigShow c TodoItem where
    configShow item = (startFormat ∷ Formatter c) <++> (printM item ∷ Formatter c)
