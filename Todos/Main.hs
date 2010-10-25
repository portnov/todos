{-# LANGUAGE UnicodeSyntax #-}

module Todos.Main where

import Prelude hiding (putStrLn,readFile,getContents,print)
import IO
import Data.Tree
import Codec.Binary.UTF8.String
import System (getArgs)
import System.Exit
import System.Cmd (system)

import System.Environment

import Todos.Types
import Todos.Unicode
import Todos.Color
import Todos.Shapes
import Todos.Dates
import Todos.Dot
import Todos.CmdLine
import Todos.Tree
import Todos.ReadConfig
import Todos.Loader
import Todos.CommandParser
import Todos.Config
import Todos.ConfigUtils

todos ∷ TodosConfig → IO ()
todos tcfg = do
  currDate ← getCurrentDateTime 
  config ← readConfig
  args ← getArgs
  let pres = (parseCommandLine tcfg) currDate (nullConfig tcfg) (config ⧺ args)
  case pres of
    Parsed q files' → do
      files ← glob files'
      todos ← loadTodo q currDate files
      let queried  = (filterTodos tcfg) currDate q todos
          format item = item {itemDescr = printfItem (descrFormat q) item}
      case commandToRun q of
        JustShow  → (printTodos tcfg) q (mapT format queried)
        ShowAsDot → 
             putStrLn $ showAsDot (itemColor tcfg) (itemShape tcfg) (mapT format queried)
        SystemCommand cmd → do
             forT selected (\item → system $ printfItem cmd (format item))
             return ()
          where selected | outOnlyFirst q = [Node (rootLabel $ head queried) []]
                         | otherwise      = queried

    ParseError str → error str
    CmdLineHelp → do putStrLn usage
                     exitWith ExitSuccess


