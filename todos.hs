{-# LANGUAGE UnicodeSyntax, PatternGuards #-}

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

pruneByDefault = Limit 20

data Limit = Unlimited
           | Limit ℤ
  deriving (Eq,Show)

instance Ord Limit where
    compare Unlimited Unlimited = EQ
    compare Unlimited _ = GT
    compare _ Unlimited = LT
    compare (Limit x) (Limit y) = compare x y

data Flag = Tag String
          | Name String
          | Status String
          | Prune ℤ
          | AndCons
          | OrCons
          | NotCons
          | NoFilter
          | HelpF
     deriving (Eq,Ord,Show)         

data Query = Query Limit Composed
           | Help
    deriving (Eq,Show)

data Composed = Pred Flag
              | And Composed Composed
              | Or Composed Composed
              | Not Composed
              | Empty
              | HelpC
    deriving (Eq,Show)

compose ∷  Composed → (TodoItem → Bool)
compose Empty             = const True
compose (Pred NoFilter)   = const True
compose (Pred (Tag s))    = tagPred s
compose (Pred (Name s))   = grepPred s
compose (Pred (Status s)) = statusPred s
compose (Not p)           = not ∘ (compose p)
compose (And (Pred NoFilter) p) = compose p
compose (And p (Pred NoFilter)) = compose p
compose (And p1 p2)      = \item → (compose p1 item) ∧ (compose p2 item)
compose (Or (Pred NoFilter) p) = compose p
compose (Or p (Pred NoFilter)) = compose p
compose (Or p1 p2)       = \item → (compose p1 item) ∨ (compose p2 item)
compose x = error $ show x

composeAll ∷ Query → ([Todo] → [Todo])
composeAll q = mkSelector (limit q) $ compose $ query q
  where
    limit (Query (Limit x) _) = x
    query (Query _ c)         = c
    mkSelector n p = concatMap $ pruneSelector n p

emptyC ∷ Composed
emptyC = Pred NoFilter

appendC ∷ Composed → Flag → Composed
appendC (Not (Pred NoFilter))   f = Not (Pred f)
appendC Empty OrCons              = (Pred NoFilter) `Or` (Pred NoFilter)
appendC Empty AndCons             = (Pred NoFilter) `And` (Pred NoFilter)
appendC Empty NotCons             = Not (Pred NoFilter)
appendC Empty f                   = Pred f
appendC c NoFilter                = c
appendC _ HelpF                   = HelpC
appendC c AndCons                 = c `And` (Pred NoFilter)
appendC c OrCons                  = c `Or`  (Pred NoFilter)
appendC c NotCons                 = c `And` (Pred NoFilter)
appendC (And c (Pred NoFilter)) f = c `And` (Pred f) 
appendC (And (Pred NoFilter) c) f = c `And` (Pred f) 
appendC c@(And _ _)             f = c `And` (Pred f)
appendC (Or c (Pred NoFilter))  f = c `Or`  (Pred f)
appendC (Or (Pred NoFilter) c)  f = c `Or`  (Pred f)
appendC c@(Or _ _)              f = c `Or`  (Pred f)
appendC c@(Pred _)              f = c `And` (Pred f)
appendC c                       f = c `And` (Pred f)

concatC ∷ [Flag] → Query
concatC flags | HelpC ← composedFlags = Help
              | otherwise             = Query limit composedFlags
  where
    composedFlags = foldl appendC Empty (queryFlags)
    queryFlags    = filter (not ∘ isPrune) flags
    limit1        = foldl min Unlimited $ map unPrune pruneFlags
    limit | Unlimited ← limit1 = pruneByDefault
          | otherwise          = limit1
    pruneFlags    = filter isPrune flags
    isPrune (Prune _) = True
    isPrune _         = False
    unPrune (Prune x) = Limit x

parseCmdLine ∷  IO ([Flag], FilePath)
parseCmdLine = do
  args ← getArgs
  return $ case getOpt RequireOrder options (map decodeString args) of
        (flags, [],      [])     → (flags,"TODO")
        (flags, nonOpts, [])     → (flags, head nonOpts)
        (_,     _,       msgs)   → error $ concat msgs ⧺ usage

usage ∷  String
usage = usageInfo header options
  where 
    header = "Usage: todos [OPTION...] [INPUTFILE]"

options ∷  [OptDescr Flag]
options = [
    Option "p" ["prune"]  (ReqArg mkPrune "N")     "limit tree height to N",
    Option "t" ["tag"]    (ReqArg Tag "TAG")       "find items marked with TAG",
    Option "g" ["grep"]   (ReqArg Name "PATTERN")  "find items with PATTERN in name",
    Option "s" ["status"] (ReqArg Status "STRING") "find items with status equal to STRING",
    Option "a" ["and"]    (NoArg AndCons)          "logical AND",
    Option "o" ["or"]     (NoArg OrCons)           "logical OR",
    Option "n" ["not"]    (NoArg NotCons)          "logical NOT",
    Option "h" ["help"]   (NoArg HelpF)            "display this help"
  ]

mkPrune ∷  String → Flag
mkPrune s = Prune (read s)

main ∷  IO ()
main = do
  (flags, file) ← parseCmdLine
  let opt = concatC flags
--   print opt
  case opt of
    Help → do putStrLn usage
              exitWith ExitSuccess
    c → do
      todos ← loadTodo file
      let todos' = delTag "-" todos
      putStrLn $ showTodos $ composeAll c todos'

