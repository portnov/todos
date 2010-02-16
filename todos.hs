{-# LANGUAGE UnicodeSyntax, PatternGuards #-}

import Prelude hiding (putStrLn,readFile,getContents,print)
import IO
import Codec.Binary.UTF8.String
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
  currDate ← getCurrentDateTime 
  config ← readConfig
  args ← getArgs
  let (loptions, files') = parseCmdLine currDate (config ⧺ args)
  files ← glob files'
  case loptions of
    O qflags mflags oflags lflags → 
      do
        let options = O qflags mflags oflags lflags
            q = buildQuery options
        todos ← loadTodo (prefix q) currDate files
        let todos'  = delTag "-" todos
            queried = transformList q (composeAll currDate) todos'
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

