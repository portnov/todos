{-# LANGUAGE UnicodeSyntax #-}
module Todos.ConfigUtils where

import Control.Monad.Reader
import System.Console.ANSI

import Todos.Unicode
import Todos.Types
import Todos.Config
import Todos.Color
import Todos.Shapes
import Todos.CmdLine
import Todos.Tree

-- | Default empty Config (nullConfig field of defaultConfig)
emptyConfig = Config {
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
  topStatus = Nothing,
  query = Empty }

-- | Default Todos config
defaultConfig ∷ TodosConfig
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
composeAll ∷ DateTime → ListTransformer
composeAll date = do
  pred ← asks ((compose date) ∘ query)
  concatMap `fmap` pruneSelector pred

-- | Default filter for TODOs (filterTodos field of defaultConfig)
defaultTodosFilter ∷ DateTime → Config → [Todo] → [Todo]
defaultTodosFilter dt conf todos =
  let t = delTag "-" todos
  in  if outHighlight conf
        then t
        else transformList conf (composeAll dt) t

transformList ∷  r → Reader r (t → a) → t → a
transformList conf tr list = do
    f ← tr
    return (f list)
  `runReader` conf

-- | Parse command line
parseCmdLine ∷ DateTime              -- ^ Current date/time
             → Config                -- ^ Default config
             → [String]              -- ^ Command line args
             → CmdLineParseResult
parseCmdLine currDate dc args = 
  case parseCmdLine' currDate args of
    Right (opts, files) → case opts of
                           Help → CmdLineHelp
                           _    → Parsed (buildQuery dc opts) files
    Left str            → ParseError str

mkPrintConfig ∷ DateTime → Config → TodosConfig → PrintConfig
mkPrintConfig dt conf tcfg = PConfig {
  printConfig      = conf,
  printStatusColor = statusConsoleColor tcfg,
  printItemColor   = itemConsoleColor tcfg,
  printHighlightColor = highlightColor tcfg,
  doHighlight      = mkHighlightFn dt conf }

mkHighlightFn ∷ DateTime → Config → TodoItem → 𝔹
mkHighlightFn dt conf = compose dt $ query conf

