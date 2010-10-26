{-# LANGUAGE UnicodeSyntax, MultiParamTypeClasses #-}
module Todos.ConfigUtils where

import Control.Monad.Reader
import System.Console.ANSI

import Todos.Unicode
import Todos.Types
import Todos.Config
import Todos.Color
import Todos.Shapes
import Todos.CmdLine
import Todos.Print
import Todos.Tree

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
  descrFormat = "%d",
  skipStatus = False,
  groupByFile = False,
  groupByTag = False,
  groupByStatus = False,
  forcedStatus = Nothing,
  topStatus = Nothing
}

-- | Default empty Config (nullConfig field of defaultConfig)
emptyConfig = Config {
  baseConfig = emptyBaseConfig,
  query = Empty }

-- | Default Todos config
defaultConfig ∷ TodosConfig Config
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
composeAll ∷ DateTime → Config → (Todo → [Todo])
composeAll date conf =
  let pred = compose date $ query conf
      bc = baseConfig conf
  in  pruneSelector bc pred

-- | Default filter for TODOs (filterTodos field of defaultConfig)
defaultTodosFilter ∷ DateTime → Config → [Todo] → [Todo]
defaultTodosFilter dt conf todos =
  let t = delTag "-" todos
      bc = baseConfig conf
  in  if outHighlight bc
        then t
        else concatMap (composeAll dt conf) t

-- | Parse command line
parseCmdLine ∷ DateTime              -- ^ Current date/time
             → Config                -- ^ Default config
             → [String]              -- ^ Command line args
             → CmdLineParseResult Config
parseCmdLine currDate dc args = 
  case parseCmdLine' currDate args of
    Right (opts, files) → case opts of
                           Help → CmdLineHelp
                           _    → Parsed (buildQuery (baseConfig dc) opts) files
    Left str            → ParseError str

mkPrintConfig ∷ (QueryConfig c) ⇒ DateTime → c → TodosConfig c → PrintConfig c
mkPrintConfig dt conf tcfg = PConfig {
  printConfig      = conf,
  printStatusColor = statusConsoleColor tcfg,
  printItemColor   = itemConsoleColor tcfg,
  printHighlightColor = highlightColor tcfg,
  doHighlight      = getPredicate dt conf }

