{-# LANGUAGE UnicodeSyntax #-}

module Todos.Default.Curses (cursesPrintTodos) where

import Control.Monad
import Control.Monad.Reader
import Data.Tree
import System.Console.ANSI as ANSI
import UI.HSCurses.Curses  as Curses
import System.Locale.SetLocale

import Todos.Types
import Todos.Default.Config
import Todos.Default.Print (showTodos)
import Todos.Formatters hiding (outItem)

toCursesColor ∷ ANSI.Color → Curses.Color
toCursesColor ANSI.Black   = Curses.Color 0
toCursesColor ANSI.Red     = Curses.Color 1
toCursesColor ANSI.Green   = Curses.Color 2
toCursesColor ANSI.Yellow  = Curses.Color 3
toCursesColor ANSI.Blue    = Curses.Color 4
toCursesColor ANSI.Magenta = Curses.Color 5
toCursesColor ANSI.Cyan    = Curses.Color 6
toCursesColor ANSI.White   = Curses.Color 7

setColor ∷ Window → ANSI.ColorIntensity → ANSI.Color → IO ()
setColor pad int clr = do
  let (Curses.Color c) = toCursesColor clr
  (attr, _) ← wAttrGet pad
  let attr' = case int of
               ANSI.Vivid → Curses.setBold attr True
               ANSI.Dull  → attr
  wAttrSet pad (attr', Pair c)

setBoldW ∷ Window → IO ()
setBoldW pad = do
  (attr, pair) ← wAttrGet pad
  wAttrSet pad (Curses.setBold attr True, pair)
  
reset ∷ Window → IO ()
reset pad = wAttrSet pad (attr0, Pair 0)

-- | Print OutItem to console
outItem ∷  Window → OutItem → IO ()
outItem pad (OutString s)     = wAddStr pad s
outItem pad (OutSetColor i c) = setColor pad i c
outItem pad SetBold           = setBoldW pad
outItem pad ResetAll          = reset pad

cursesPrintTodos ∷ PrintConfig DefaultConfig → [Todo] → IO ()
cursesPrintTodos cfg lst = do
  setLocale LC_ALL Nothing
  initCurses
  np ← colorPairs
  nc ← colors
  let m = min np nc
  forM_ [0..m-1] $ \i →
    initPair (Pair i) (Curses.Color i) (Curses.Color (-1))
  (lines,cols) ← scrSize
  let nLines = todoLines lst
  pad ← newPad nLines cols
  let lst' = runReader (showTodos lst) cfg
  forM lst' (outItem pad)
  echo False
  scrollPad pad nLines lines cols
  delWin pad
  endWin
  
todoLines ∷ [Todo] → Int
todoLines todos = sum $ map todoLines' todos
  where
    todoLines' (Node item children) = 1 + (sum $ map todoLines' children)
    
scrollPad ∷ Window → Int → Int → Int → IO ()
scrollPad pad padLines lines cols = do
    scrollToLine 0
    waitForKeys 0
  where
    scrollToLine i = pRefresh pad i 0 0 0 (lines-1) (cols-1)
    
    scroll i = scrollToLine i >> waitForKeys i
    
    moveDown i = scroll $ min (padLines - lines) (i+1)
               
    moveUp i = scroll $ max 0 (i-1)

    waitForKeys i = do
      c ← getCh
      case c of
        KeyChar 'q' → return ()
        KeyChar 'j' → moveDown i
        KeyChar 'k' → moveUp i
        KeyChar 'g' → scroll 0
        KeyChar 'G' → scroll (padLines - lines)
        _           → beep >> waitForKeys i
