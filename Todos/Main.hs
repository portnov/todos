{-# LANGUAGE UnicodeSyntax #-}

module Todos.Main
  (module Todos.Default,
   module Todos.Config,
   module Todos.Dot,
   todos
  ) where

import Prelude hiding (putStrLn,readFile,getContents,print)
import IO
import Data.Tree
import System.Exit
import System.Cmd (system)

import System.Environment
import Config.Dyre

import Todos.Types
import Todos.Unicode
import Todos.Dates
import Todos.Dot
import Todos.Tree
import Todos.ReadConfig
import Todos.Loader
import Todos.CommandParser
import Todos.Config
import Todos.Default

-- | Sort command line arguments:
-- (should we read other configs, command-line specified config files, command-line options)
sortCmdLine ∷ [String] → (𝔹, [String], [String])
sortCmdLine args = foldr sortOne (True, [],[]) args
  where
    sortOne "@@"           (_, configs, as) = (False, configs, as)
    sortOne ('@':'@':path) (_, configs, as) = (False, path:configs, as)
    sortOne ('@':path)     (r, configs, as) = (r, path:configs, as)
    sortOne option         (r, configs, as) = (r, configs, option:as)

-- | Real main funciton. Is called by dyre.
realTodos ∷ (RuntimeConfig c) ⇒ TodosConfig c → IO ()
realTodos tcfg = do
  currDate ← getCurrentDateTime 
  args ← getArgs
  let (readOther, configs, args') = sortCmdLine args
  config ← if readOther
             then readAllConfigs
             else return []
  -- Read options from command-line specified config files
  cmdLineConfig ← concat `fmap` mapM readConfigFile configs
  let pres = (parseCommandLine tcfg) currDate (nullConfig tcfg) (config ⧺ cmdLineConfig ⧺ args')
  case pres of
    Parsed q files' → do
      let bc = toBaseConfig q
      files ← glob files'
      todos ← loadTodo bc currDate files
      let queried  = enumerateTodos $ (filterTodos tcfg) currDate q todos
      case commandToRun bc of
        JustShow  → printTodos tcfg (mkPrintConfig currDate q tcfg) queried
        ShowAsDot → 
             putStrLn $ showAsDot (itemColor tcfg) (itemShape tcfg) queried
        SystemCommand cmd → do
             forT selected (\item → system $ printfItem cmd item)
             return ()
          where selected | outOnlyFirst bc = [Node (rootLabel $ head queried) []]
                         | otherwise       = queried

    ParseError str → error str
    CmdLineHelp → do putStrLn usage
                     exitWith ExitSuccess

-- | Main function to run. User can specify TodosConfig with any runtime config
-- type. By default (in todos.hs) defaultConfig is used, which uses DefaultConfig type.
todos ∷ (RuntimeConfig c) ⇒ TodosConfig c → IO ()
todos = wrapMain $ defaultParams {
    projectName = "todos",
    realMain    = realTodos,
    statusOut   = const (return ())
    }

