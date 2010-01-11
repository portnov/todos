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
import Types
import TodoParser
import TodoTree

pruneByDefault = Limit 20

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
composeAll q = mkSelector (pruneL q) (minL q) $ compose $ query q
  where
    mkSelector (Limit n) (Limit m) p = concatMap $ pruneSelector n m p

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

concatC ∷ [Flag] → (Query, 𝔹)
concatC flags | HelpC ← composedFlags = (Help, undefined)
              | otherwise             = (Query limitP limitM composedFlags, onlyFirst)
  where
    composedFlags = foldl appendC Empty (queryFlags)
    queryFlags    = filter isQuery flags

    limitP'       = foldl min Unlimited $ map unPrune pruneFlags
    limitP | Unlimited ← limitP' = pruneByDefault
           | otherwise           = limitP'

    limitM'       = foldl max (Limit 0) $ map unMin minFlags
    limitM | Unlimited ← limitM' = pruneByDefault
           | otherwise           = limitM'

    pruneFlags = filter isPrune flags
    minFlags   = filter isMin   flags
    onlyFirst  = not $ null $ filter isFirst flags

    isQuery x         = (not $ isPrune x) ∧ (not $ isMin x) ∧ (not $ isFirst x)

    isPrune (Prune _) = True
    isPrune _         = False

    isMin   (Start x) = True
    isMin   _         = False

    isFirst OnlyFirst = True
    isFirst _         = False

    unMin   (Start x) = Limit x
    unPrune (Prune x) = Limit x

parseCmdLine ∷  IO ([Flag], [FilePath])
parseCmdLine = do
  args ← getArgs
  return $ case getOpt RequireOrder options (map decodeString args) of
        (flags, [],      [])     → (flags, ["TODO"])
        (flags, nonOpts, [])     → (flags, nonOpts)
        (_,     _,       msgs)   → error $ concat msgs ⧺ usage

usage ∷  String
usage = usageInfo header options
  where 
    header = "Usage: todos [OPTION...] [INPUT FILES]"

options ∷  [OptDescr Flag]
options = [
    Option "1" ["only-first"] (NoArg OnlyFirst)    "show only first matching entry",
    Option "p" ["prune"]  (ReqArg mkPrune "N")     "limit tree height to N",
    Option "m" ["min-depth"] (ReqArg mkMin "N")    "show first N levels of tree unconditionally",
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

mkMin s = Start (read s)

main ∷  IO ()
main = do
  (flags, files) ← parseCmdLine
  let (opt, onlyFirst) = concatC flags
--   print opt
  case opt of
    Help → do putStrLn usage
              exitWith ExitSuccess
    c → do
      todos ← loadTodo files
      let todos' = delTag "-" todos
      putStrLn $ showTodos onlyFirst $ composeAll c todos'

