{-# LANGUAGE UnicodeSyntax, PatternGuards #-}

-- import Prelude hiding (putStrLn,print)
import Codec.Binary.UTF8.String
-- import System.IO.UTF8
import System (getArgs)
import System.Exit
import System.Cmd (system)

import Control.Monad.Reader
import Data.Maybe
import Data.Tree
import Data.List

import Unicode
import Types
import TodoLoader
import TodoTree
import CommandParser
import Config
import CmdLine
import Dates (getCurrentDateTime)

main ∷  IO ()
main = do
  (defPrefix, defExec, O gqflags gmflags goflags glflags) ← readConfig
  (loptions, files) ← parseCmdLine defPrefix defExec
  case loptions of
    O lqflags lmflags loflags llflags → 
      do
        currDate ← getCurrentDateTime 
        let options = O (gqflags⧺lqflags) (gmflags⧺lmflags) (goflags⧺loflags) (glflags⧺llflags)
            q = buildQuery options
        todos ← loadTodo (prefix q) currDate files
        let todos'  = delTag "-" todos
            queried = transformList q composeAll todos'
            format item = item {itemDescr = printfItem (descrFormat q) item}
        case commandToRun q of
          Nothing  → do
               printTodos q (mapT format queried)
               putStrLn ""
          Just cmd → do
               forT selected (\item → system $ printfItem cmd (format item))
               return ()
            where selected | outOnlyFirst q = [Node (rootLabel $ head queried) []]
                           | otherwise       = queried

    Help → do putStrLn usage
              exitWith ExitSuccess

