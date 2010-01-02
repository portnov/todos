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

data PrunedFilter = Bound Limit Filter
    deriving (Eq,Show)

data Cutter = Only PrunedFilter
              | And PrunedFilter Cutter
              | Or PrunedFilter Cutter
              | PassAll
              | MakeOr
              | Help
    deriving (Eq,Show)

data Filter = Tag String
            | Name String
            | Status String
            | Prune ℤ
            | Unlimit
            | AndCons
            | OrCons
            | NoFilter
            | HelpF
     deriving (Eq,Ord,Show)         

data Composed = Pred Filter
              | AndF Composed Composed
              | OrF Composed Composed
    deriving (Eq,Show)

pred1 (Bound (Limit n) (Tag s)) = (n, Pred $ Tag s)
pred1 (Bound (Limit n) (Name s)) = (n, Pred $ Name s)
pred1 (Bound (Limit n) (Status s)) = (n, Pred $ Status s)
pred1 (Bound (Limit n) NoFilter) = (n, Pred $ NoFilter)
pred1 x = error $ show x

composePred ∷ (ℤ, Composed) → Cutter → (ℤ, Composed)
composePred (n,p) (Only f) | OrF (Pred NoFilter) p2 <- p = (m, p1 `OrF` p2)
                           | otherwise          = (m, p `AndF` p1)
  where
    (n1,p1) = pred1 f
    m = min n1 n
composePred (n,p) (And f c) = (m, p `AndF` p1) `composePred` c
  where
    (n1,p1) = pred1 f
    m = min n1 n
composePred (n,p) (Or f c) = (m, p `OrF` p1) `composePred` c
  where
    (n1,p1) = pred1 f
    m = min n1 n

preds ∷ Cutter → (ℤ, Composed)
preds c = composePred (20, Pred NoFilter) c

compose' (Pred NoFilter)   = const True
compose' (Pred (Tag s))    = tagPred s
compose' (Pred (Name s))   = grepPred s
compose' (Pred (Status s)) = statusPred s
compose' (AndF (Pred NoFilter) p) = compose' p
compose' (AndF p (Pred NoFilter)) = compose' p
compose' (AndF p1 p2)      = \item → (compose' p1 item) ∧ (compose' p2 item)
compose' (OrF (Pred NoFilter) p) = compose' p
compose' (OrF p (Pred NoFilter)) = compose' p
compose' (OrF p1 p2)       = \item → (compose' p1 item) ∨ (compose' p2 item)
compose' x = error $ show x

compose = fmap compose'

composeAll ∷ Cutter → ([Todo] → [Todo])
composeAll c = mkSelector $ compose $ preds c
  where
    mkSelector (n,p) = concatMap $ pruneSelector n p

emptyC = Only (Bound pruneByDefault  NoFilter)

limit (Only (Bound _ f)) n = Only (Bound (Limit n) f)
limit (Or (Bound _ f) c) n = (Bound (Limit n) f) `Or` c
limit (And (Bound _ f) c) n = (Bound (Limit n) f) `And` c

limit' (Limit n) (Only (Bound Unlimited f)) = Only (Bound (Limit n) f)
limit' (Limit n) (Or (Bound Unlimited f) c) = (Bound (Limit n) f) `Or` (limit' (Limit n) c)
limit' (Limit n) (And (Bound Unlimited f) c) = (Bound (Limit n) f) `And` (limit' (Limit n) c)
limit' _ c = c

unlimit (Only (Bound _ f)) = Only (Bound Unlimited f)
unlimit (Or (Bound _ f) c) = (Bound Unlimited f) `Or` c
unlimit (And (Bound _ f) c) = (Bound Unlimited f) `And` c

appendC c NoFilter = c
appendC _ HelpF    = Help
appendC c@(And (Bound _ _) _) (Prune n) = limit c n
appendC c@(And (Bound _ _) _) Unlimit = unlimit c
appendC (And (Bound b NoFilter) c) f = (Bound b f) `And` c
appendC c@(Or (Bound _ _) _) (Prune n) = limit c n
appendC c@(Or (Bound _ _) _) Unlimit = unlimit c
appendC (Or (Bound b NoFilter) c) f = (Bound b f) `Or` c
appendC c@(Only (Bound _ f)) (Prune n) = limit c n
appendC c@(Only (Bound _ _)) Unlimit = unlimit c
appendC (Only (Bound b NoFilter)) f = Only (Bound b f)
appendC c AndCons = (Bound Unlimited NoFilter) `And` c
appendC c OrCons = (Bound Unlimited NoFilter) `Or` c
appendC c@(And _ _) f = (Bound Unlimited f) `And` c
appendC c@(Or _ _) f = (Bound Unlimited f) `Or` c
appendC c f        = (Bound Unlimited f) `And` c

parseCmdLine = do
  args ← getArgs
  return $ case getOpt RequireOrder options (map decodeString args) of
        (flags, [],      [])     → (flags,"TODO")
        (flags, nonOpts, [])     → (flags, head nonOpts)
        (_,     _,       msgs)   → error $ concat msgs ⧺ usage

usage = usageInfo header options

header = "Usage: todos [OPTION...] [INPUTFILE]"

options = [
    Option "p" ["prune"] (ReqArg mkPrune "N") "limit tree height to N",
    Option "u" ["unlimit"] (NoArg Unlimit)    "unlimit tree height",
    Option "t" ["tag"]   (ReqArg mkTag "TAG") "find items marked with TAG",
    Option "ng" ["name","grep"] (ReqArg mkName "PATTERN") "find items with PATTERN in name",
    Option "s" ["status"] (ReqArg mkStatus "STRING") "find items with status equal to STRING",
    Option "a" ["and"]  (NoArg AndCons)       "logical AND",
    Option "o" ["or"]   (NoArg OrCons)        "logical OR",
    Option "h" ["help"] (NoArg HelpF) "display this help"
  ]

mkPrune s = Prune (read s)
mkTag t = Tag t
mkName n = Name n
mkStatus s = Status s

main = do
  (cutters, file) ← parseCmdLine
--   print cutters
  let opt = limit' pruneByDefault $ foldl appendC emptyC (reverse cutters)
--   print opt
  case opt of
    Help → do putStrLn usage
              exitWith ExitSuccess
    c → do
--       print $ preds c
      todos ← loadTodo file
      let todos' = delTag "-" todos
      putStrLn $ showTodos $ composeAll c todos'
  

