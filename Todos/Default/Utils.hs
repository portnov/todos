{-# LANGUAGE UnicodeSyntax, MultiParamTypeClasses #-}
-- | This module contains some empty configs definitions and some function fields of defaultConfig
module Todos.Default.Utils where

import System.Console.ANSI

import Todos.Types
import Todos.Dot
import Todos.Default.CmdLine
import Todos.Default.Config
import Todos.Default.Print
import Todos.Tree

-- | Empty BaseConfig
emptyBaseConfig ∷ BaseConfig
emptyBaseConfig = BConfig {
  outOnlyFirst = False,
  outColors = False,
  outIds = False,
  outHighlight = False,
  sorting = DoNotSort,
  pruneL = Limit 20,
  minL = Limit 0,
  commandToRun = JustShow,
  prefix = Nothing,
  outputFormat = "%s %D%t%n  %d",
  indentString = "  ",
  skipStatus = False,
  groupByFile = False,
  groupByTag = False,
  groupByStatus = False,
  forcedStatus = Nothing,
  topStatus = Nothing
}

-- | Default empty DefaultConfig (nullConfig field of defaultConfig)
emptyConfig ∷ DefaultConfig
emptyConfig = DConfig {
  baseConfig = emptyBaseConfig,
  query = Empty }

-- | Default Todos config
defaultConfig ∷ TodosConfig DefaultConfig
defaultConfig = Todos {
  parseCommandLine = parseCmdLine,
  filterTodos = defaultTodosFilter,
  statusConsoleColor = statusColor,
  itemConsoleColor = defItemConsoleColor,
  highlightColor = (Vivid, Magenta),
  itemColor = getColor,
  itemShape = getShape,
  printTodos = defaultPrintTodos,
  nullConfig = emptyConfig
}

-- | Make a list transformer
composeAll ∷ DateTime → DefaultConfig → (Todo → [Todo])
composeAll date conf =
  let pred = compose date $ query conf
      bc = baseConfig conf
  in  pruneSelector bc pred

-- | Default filter for TODOs (filterTodos field of defaultConfig)
defaultTodosFilter ∷ DateTime → DefaultConfig → [Todo] → [Todo]
defaultTodosFilter dt conf todos =
  let t = delTag "-" todos
      bc = baseConfig conf
  in  if outHighlight bc
        then t
        else concatMap (composeAll dt conf) t

-- | Parse command line (default function)
parseCmdLine ∷ DateTime              -- ^ Current date/time
             → DefaultConfig         -- ^ Default config
             → [String]              -- ^ Command line args
             → CmdLineParseResult DefaultConfig
parseCmdLine currDate dc args = 
  case parseCmdLine' currDate args of
    Right (opts, files) → case opts of
                           Help → CmdLineHelp
                           _    → Parsed (buildQuery (baseConfig dc) opts) files
    Left str            → ParseError str

-- | Prepare PrintConfig for console output functions. Is called from realTodos.
mkPrintConfig ∷ (RuntimeConfig c) ⇒ DateTime → c → TodosConfig c → PrintConfig c
mkPrintConfig dt conf tcfg = PConfig {
  printConfig      = conf,
  printStatusColor = statusConsoleColor tcfg,
  printItemColor   = itemConsoleColor tcfg,
  printHighlightColor = highlightColor tcfg,
  doHighlight      = getPredicate dt conf }

