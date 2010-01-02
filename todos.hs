{-# LANGUAGE UnicodeSyntax #-}

import Prelude hiding (putStrLn,print)
import Codec.Binary.UTF8.String
import System.IO.UTF8
import System (getArgs)
import System.Exit
import System.Console.GetOpt

import Control.Monad
import Data.Monoid
import Data.List

import Unicode
import TodoParser
import TodoTree

pruneByDefault = Bound 20

data Bound = NoBound
           | Bound ℤ
  deriving (Eq,Show)

instance Ord Bound where
    compare NoBound NoBound = EQ
    compare NoBound _ = GT
    compare _ NoBound = LT
    compare (Bound x) (Bound y) = compare x y

data Cutter = Prune Bound [Filter]
            | Help
    deriving (Eq,Show)

data Filter = Tag String
            | Name String
            | Status String
     deriving (Eq,Ord,Show)         

instance Monoid Cutter where
  mempty = Prune pruneByDefault []
  mappend Help _ = Help
  mappend _ Help = Help
  mappend (Prune m1 lst1) (Prune m2 lst2) = Prune (min m1 m2) $ sort $ lst1 ⧺ lst2

parseCmdLine = do
  args ← getArgs
  return $ case getOpt RequireOrder options (map decodeString args) of
        (flags, [],      [])     → (flags,"todo.txt")
        (flags, nonOpts, [])     → (flags, head nonOpts)
        (_,     _,       msgs)   → error $ concat msgs ⧺ usage

usage = usageInfo header options

header = "Usage: todos [OPTION...]"

options = [
    Option "p" ["prune"] (ReqArg mkPrune "N") "limit tree height to N",
    Option "t" ["tag"]   (ReqArg mkTag "TAG") "find items marked with TAG",
    Option "ng" ["name","grep"] (ReqArg mkName "PATTERN") "find items with PATTERN in name",
    Option "s" ["status"] (ReqArg mkStatus "STRING") "find items with status equal to STRING",
    Option "h" ["help"] (NoArg Help) "display this help"
  ]

mkPrune s = Prune (Bound $ read s) []
mkTag t = Prune NoBound [Tag t]
mkName n = Prune NoBound [Name n]
mkStatus s = Prune NoBound [Status s]

selectFilter n (Tag s) = findTag n s
selectFilter n (Name s) = grep n s
selectFilter n (Status s) = filterStatus n s

main = do
  (cutters, file) ← parseCmdLine
  let opt = mconcat cutters
  case opt of
    Help → do putStrLn usage
              exitWith ExitSuccess
    Prune (Bound n) filters → do
      let list = case filters of
                  [] → [prune n]
                  fs → map (selectFilter n) fs
          fun = foldr (∘) id list
      todos ← loadTodo file
      let todos' = delTag "-" todos
      putStrLn $ showTodos $ fun todos'
  

