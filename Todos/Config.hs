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

-- | Runtime configuration. Is read from command line and configs.
data Config = Config {
      outOnlyFirst ∷ 𝔹,           -- ^ Output only first matching entry
      outColors ∷ 𝔹,              -- ^ Show colored output
      outIds :: 𝔹,                -- ^ Show IDs
      outHighlight ∷ 𝔹,           -- ^ Highlight matching items
      sorting ∷ SortingType,      -- ^ How to sort items
      pruneL ∷ Limit, 
      minL   ∷ Limit,
      commandToRun ∷ TodoCommand,
      prefix ∷ Maybe String,      -- ^ Nothing — use default parser, Just p — use alternate parser with prefix «p»
      descrFormat ∷ String,
      skipStatus ∷ 𝔹,             -- ^ Skip status field in input
      groupByFile ∷ 𝔹,
      groupByTag ∷ 𝔹,
      groupByStatus ∷ 𝔹,
      forcedStatus ∷ Maybe String,
      topStatus ∷ Maybe String,
      query ∷ Composed }
    deriving (Eq,Show)

-- | Configuration for console output. Is generated in runtime from TodosConfig and Config.
data PrintConfig = PConfig {
  printConfig ∷ Config,
  printStatusColor ∷  String → (ANSI.ColorIntensity, ANSI.Color),       -- ^ Color of status field from status
  printItemColor ∷  TodoItem → Maybe (ANSI.ColorIntensity, ANSI.Color), -- ^ Color of item name
  printHighlightColor ∷ (ANSI.ColorIntensity, ANSI.Color),              -- ^ Color to use for highlighting
  doHighlight ∷ TodoItem → 𝔹                                            -- ^ Whether to highlight given item
  }

-- | User Todos config. User can specify it in @~/.config/todos/todos.hs@.
data TodosConfig = Todos {
     parseCommandLine ∷ DateTime → Config → [String] → CmdLineParseResult,  -- ^ Function to parse command line
     filterTodos ∷ DateTime → Config → [Todo] → [Todo],                     -- ^ Any function to be run to transform read TODOs tree
     statusConsoleColor ∷ String → (ANSI.ColorIntensity, ANSI.Color),       -- ^ Function to select a color of item's status field in console output
     itemConsoleColor ∷ TodoItem → Maybe (ANSI.ColorIntensity, ANSI.Color), -- ^ Function to select a color of item's name in console output
     highlightColor ∷ (ANSI.ColorIntensity, ANSI.Color),                    -- ^ Color to use for highlighting
     itemColor ∷ TodoItem → HSV,                                            -- ^ Function to select color for item's node in DOT output
     itemShape ∷ TodoItem → Shape,                                          -- ^ Function to select shape for item's node in DOT output
     printTodos ∷ PrintConfig → [Todo] → IO (),                             -- ^ Any function to output TODOs list
     nullConfig ∷ Config                                                    -- ^ Default Config (to be used without any options in command line and configs)
}

-- | Result of parsing command line
data CmdLineParseResult = 
     Parsed Config [FilePath]  -- ^ Parsed successfully, got Config and list of source files
   | ParseError String         -- ^ Some error occured
   | CmdLineHelp               -- ^ User asked for help
   deriving (Eq,Show)

