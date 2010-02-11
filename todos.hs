{-# LANGUAGE UnicodeSyntax, PatternGuards #-}

import Prelude hiding (putStrLn,print)
import Codec.Binary.UTF8.String
import System.IO.UTF8
import System (getArgs)
import System.Exit
import System.Cmd (system)

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

main ∷  IO ()
main = do
  (defPrefix, defExec, O gqflags gmflags glflags) <- readConfig
  (loptions, files) <- parseCmdLine defPrefix defExec
  case loptions of
    O lqflags lmflags llflags -> 
      do
        let options = O (gqflags++lqflags) (gmflags++lmflags) (glflags++llflags)
            q = buildQuery options
        todos ← loadTodo (prefix q) files
        let todos'  = delTag "-" todos
            queried = composeAll q todos'
            format item = item {itemDescr = printfItem (descrFormat q) item}
        case commandToRun q of
          Nothing  → do
               showTodos (showOnlyFirst q) (mapT format queried)
               putStrLn ""
          Just cmd → do
               forT selected (\item → system $ printfItem cmd (format item))
               return ()
            where selected | showOnlyFirst q = [Node (rootLabel $ head queried) []]
                           | otherwise       = queried

    Help -> do putStrLn usage
               exitWith ExitSuccess

