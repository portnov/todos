{-# LANGUAGE UnicodeSyntax #-}

module Todos.Config where

import Control.Monad.Reader

import Todos.Unicode
import Todos.Types
import Todos.Color
import Todos.Shapes
import Text.ParserCombinators.Parsec
import qualified System.Console.ANSI as ANSI

type ListTransformer = Reader Config ([Todo] → [Todo])

data Config = Config {
      outOnlyFirst ∷ 𝔹,
      outColors ∷ 𝔹,
      outIds :: 𝔹,
      sorting ∷ SortingType,
      pruneL ∷ Limit,
      minL   ∷ Limit,
      commandToRun ∷ TodoCommand,
      prefix ∷ Maybe String,
      descrFormat ∷ String,
      skipStatus ∷ 𝔹,
      groupByFile ∷ 𝔹,
      groupByTag ∷ 𝔹,
      groupByStatus ∷ 𝔹,
      forcedStatus ∷ Maybe String,
      topStatus ∷ Maybe String,
      query ∷ Composed }
    deriving (Eq,Show)

data PrintConfig = PConfig {
  printConfig ∷ Config,
  printStatusColor ∷  String → (ANSI.ColorIntensity, ANSI.Color),
  printItemColor ∷  TodoItem → Maybe (ANSI.ColorIntensity, ANSI.Color) }

data TodosConfig = Todos {
     parseCommandLine ∷ DateTime → Config → [String] → CmdLineParseResult,
     filterTodos ∷ DateTime → Config → [Todo] → [Todo],
     statusConsoleColor ∷ String → (ANSI.ColorIntensity, ANSI.Color),
     itemConsoleColor ∷ TodoItem → Maybe (ANSI.ColorIntensity, ANSI.Color),
     itemColor ∷ TodoItem → HSV,
     itemShape ∷ TodoItem → Shape,
     printTodos ∷ PrintConfig → [Todo] → IO (),
     nullConfig ∷ Config
}

mkPrintConfig ∷ Config → TodosConfig → PrintConfig
mkPrintConfig conf tcfg = PConfig conf (statusConsoleColor tcfg) (itemConsoleColor tcfg)

data CmdLineParseResult = 
     Parsed Config [FilePath]
   | ParseError String
   | CmdLineHelp
   deriving (Eq,Show)

