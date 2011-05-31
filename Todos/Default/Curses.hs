{-# LANGUAGE UnicodeSyntax #-}

module Todos.Default.Curses (cursesPrintTodos) where

import Control.Monad
import Control.Monad.Reader
import System.IO
import System.Console.ANSI as ANSI
import UI.HSCurses.Curses  as Curses
import System.Locale.SetLocale

import Todos.Types
import Todos.Tree
import Todos.Default.Config
import Todos.Default.Print (showTodos, defaultPrintTodos)
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

cursesPrintTodos ∷ (TodoItem → IO ()) → PrintConfig DefaultConfig → [Todo] → IO ()
cursesPrintTodos run cfg lst = do
  tty ← hIsTerminalDevice stdout
  if not tty
    then defaultPrintTodos cfg lst
    else do
      setLocale LC_ALL Nothing
      initCurses
      (lines,cols) ← scrSize
      let nLines = fromIntegral $ treeLines lst
      if nLines >= lines
        then do
          np ← colorPairs
          nc ← colors
          let m = min np nc
          forM_ [0..m-1] $ \i →
            initPair (Pair i) (Curses.Color i) (Curses.Color (-1))
          pad ← newPad nLines cols
          let lst' = runReader (showTodos lst) cfg
          forM lst' (outItem pad)
          echo False
          scrollPad pad nLines lines cols (runByNumber run lst)
          delWin pad
          endWin
        else do
          endWin
          defaultPrintTodos cfg lst

runByNumber ∷ (TodoItem → IO ()) → [Todo] → Int → IO ()
runByNumber run todos i =
  case itemByNumber todos (fromIntegral i) of
    Nothing → putStrLn $ "Not found: " ++ show i
    Just x  → run x
  
scrollPad ∷ Window → Int → Int → Int → (Int → IO ()) → IO ()
scrollPad pad padLines lines cols run = do
    wMove pad 0 0
    scrollToLine 0
    waitForKeys 0
  where
    scrollToLine i = pRefresh pad i 0 0 0 (lines-1) (cols-1)
    
    scroll i = do
      scrollToLine i
      waitForKeys i
    
    moveDown i = do
      (y,x) ← getYX pad
      wMove pad (min (padLines-1) (y+1)) x
      if y-i == lines - 1
        then scroll $ min (padLines - lines) (i+1)
        else scroll i
               
    moveUp i = do
      (y,x) ← getYX pad
      wMove pad (max 0 (y-1)) x
      if y == i
        then scroll $ max 0 (i-1)
        else scroll i

    moveRight i = do
      (y,x) ← getYX pad
      wMove pad y $ min (cols-1) (x+1)
      scroll i

    moveLeft i = do
      (y,x) ← getYX pad
      wMove pad y $ max 0 (x-1)
      scroll i

    runSelected = do
      (y,_) ← getYX pad
      wAddStr pad (show y)
      run (y+1)

    waitForKeys i = do
      c ← getCh
      case c of
        KeyChar 'q' → return ()
        KeyChar 'j' → moveDown i
        KeyChar 'k' → moveUp i
        KeyChar 'h' → moveLeft i
        KeyChar 'l' → moveRight i
        KeyChar 'g' → scroll 0
        KeyChar 'G' → scroll (padLines - lines)
        KeyChar 'e' → runSelected
        _           → beep >> waitForKeys i

