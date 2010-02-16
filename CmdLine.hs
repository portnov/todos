{-# LANGUAGE UnicodeSyntax, PatternGuards #-}

module CmdLine where

import Prelude hiding (putStrLn,print)
import Codec.Binary.UTF8.String
import System.IO.UTF8
import System (getArgs)
import System.Console.GetOpt
import Data.Maybe
import Control.Monad.Reader

import Unicode
import Types
import TodoTree
import Dates (parseDate)

pruneByDefault = Limit 20

compose ∷ DateTime → Composed → (TodoItem → 𝔹)
compose _ Empty             = const True
compose _ (Pred NoFilter)   = const True
compose _ (Pred (Tag s))    = tagPred s
compose _ (Pred (Name s))   = grepPred s
compose _ (Pred (Status s)) = statusPred s
compose dt (Pred (StartDateIs d)) = datePred startDate dt d
compose dt (Pred (EndDateIs d)) = datePred endDate dt d
compose dt (Pred (DeadlineIs d)) = datePred deadline dt d
compose dt (Not p)           = not ∘ (compose dt p)
compose dt (And (Pred NoFilter) p) = compose dt p
compose dt (And p (Pred NoFilter)) = compose dt p
compose dt (And p1 p2)      = \item → (compose dt p1 item) ∧ (compose dt p2 item)
compose dt (Or (Pred NoFilter) p) = compose dt p
compose dt (Or p (Pred NoFilter)) = compose dt p
compose dt (Or p1 p2)       = \item → (compose dt p1 item) ∨ (compose dt p2 item)
compose _ x = error $ show x

concatMapM ∷ (Monad m) ⇒ m (t → [t]) → m ([t] → [t])
concatMapM m = do
  f ← m
  return $ concatMap f 

composeAll ∷ DateTime → ListTransformer
composeAll date = do
  pred ← asks ((compose date) ∘ query)
  concatMapM (pruneSelector pred)

appendC ∷ Composed → QueryFlag → Composed
appendC (Not (Pred NoFilter))   f = Not (Pred f)
appendC Empty OrCons              = (Pred NoFilter) `Or` (Pred NoFilter)
appendC Empty AndCons             = (Pred NoFilter) `And` (Pred NoFilter)
appendC Empty NotCons             = Not (Pred NoFilter)
appendC Empty f                   = Pred f
appendC c NoFilter                = c
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

appendF (O q m o l) (QF f) = O (f:q) m o l
appendF (O q m o l) (MF f) = O q (f:m) o l
appendF (O q m o l) (OF f) = O q m (f:o) l
appendF (O q m o l) (LF f) = O q m o (f:l)
appendF _ HelpF = Help

parseFlags :: [CmdLineFlag] -> Options
parseFlags lst | HelpF ∈ lst = Help
parseFlags [] = O [] [] [] []
parseFlags (f:fs) = (parseFlags fs) `appendF` f

buildQuery :: Options -> Config
buildQuery (O qflags mflags oflags lflags) = Config onlyFirst colors limitP limitM command aprefix dformat composedFlags 
  where
    composedFlags = parseQuery qflags
    (limitP,limitM) = parseLimits lflags

    onlyFirst = OnlyFirst ∈ oflags
    colors = Colors ∈ oflags
    cmdFlags  = filter isCommand mflags
    command | null cmdFlags = Nothing
            | otherwise     = Just $ unExecute (last cmdFlags)

    prefixFlags = filter isPrefix mflags
    aprefix | null prefixFlags = Nothing
            | otherwise        = Just $ unPrefix (last prefixFlags)

    dflags = filter isDescribe mflags
    dformat | null dflags = "%d"
            | otherwise   = unDescribe $ last dflags

    isDescribe (Describe _) = True
    isDescribe _            = False
    isCommand (Execute _) = True
    isCommand _           = False
    isPrefix (Prefix _) = True
    isPrefix _          = False

parseLimits :: [LimitFlag] -> (Limit,Limit)
parseLimits flags = (limitP,limitM)
  where
    pruneFlags = filter isPrune flags
    minFlags   = filter isMin flags

    limitP'       = foldl min Unlimited $ map (Limit ∘ unPrune) pruneFlags
    limitP | Unlimited ← limitP' = pruneByDefault
           | otherwise           = limitP'

    limitM'       = foldl max (Limit 0) $ map (Limit ∘ unMin) minFlags
    limitM | Unlimited ← limitM' = pruneByDefault
           | otherwise           = limitM'

    isPrune (Prune _) = True
    isPrune _         = False

    isMin   (Start x) = True
    isMin   _         = False

parseQuery ∷ [QueryFlag] → Composed
parseQuery flags = foldl appendC Empty flags

parseCmdLine ∷ DateTime → [String] → (Options, [FilePath])
parseCmdLine currDate args = 
  case getOpt RequireOrder (options currDate) (map decodeString args) of
        (flags, [],      [])     → (parseFlags flags, ["TODO"])
        (flags, nonOpts, [])     → (parseFlags flags, nonOpts)
        (_,     _,       msgs)   → error $ concat msgs ⧺ usage

usage ∷  String
usage = usageInfo header (options undefined)
  where 
    header = "Usage: todos [OPTION...] [INPUT FILES]"

options ∷ DateTime → [OptDescr CmdLineFlag]
options currDate = [
    Option "1" ["only-first"] (NoArg (OF OnlyFirst))    "show only first matching entry",
    Option "c" ["color"]  (NoArg (OF Colors))    "show colored output",
    Option "A" ["prefix"] (OptArg mkPrefix "PREFIX") "use alternate parser: read only lines starting with PREFIX",
    Option "D" ["describe"] (OptArg mkDescribe "FORMAT") "use FORMAT for descriptions",
    Option "p" ["prune"]  (ReqArg mkPrune "N")     "limit tree height to N",
    Option "m" ["min-depth"] (ReqArg mkMin "N")    "show first N levels of tree unconditionally",
    Option "t" ["tag"]    (ReqArg mkTag "TAG")       "find items marked with TAG",
    Option "g" ["grep"]   (ReqArg mkName "PATTERN")  "find items with PATTERN in name",
    Option "s" ["status"] (ReqArg mkStatus "STRING") "find items with status equal to STRING",
    Option "a" ["and"]    (NoArg (QF AndCons))          "logical AND",
    Option "o" ["or"]     (NoArg (QF OrCons))           "logical OR",
    Option "n" ["not"]    (NoArg (QF NotCons))          "logical NOT",
    Option "e" ["exec"]   (OptArg mkExecute "COMMAND") "run COMMAND on each matching entry",
    Option "S" ["start-date"] (ReqArg (mkStartDate currDate) "DATE") "find items with start date bounded with DATE",
    Option "E" ["end-date"] (ReqArg (mkEndDate currDate) "DATE") "find items with end date bounded with DATE",
    Option "d" ["deadline"] (ReqArg (mkDeadline currDate) "DATE") "find items with deadline bounded with DATE",
    Option "h" ["help"]   (NoArg HelpF)            "display this help"
  ]

mkTag t = QF $ Tag t

mkName n = QF $ Name n

mkStatus s = QF $ Status s

forceEither (Right x) = x
forceEither (Left x) = error $ show x

mkStartDate dt s = QF $ StartDateIs $ forceEither $ parseDate dt s
mkEndDate dt s = QF $ EndDateIs $ forceEither $ parseDate dt s
mkDeadline dt s = QF $ DeadlineIs $ forceEither $ parseDate dt s

mkDescribe Nothing = MF $ Describe "%d"
mkDescribe (Just f) = MF $ Describe f

mkPrune ∷  String → CmdLineFlag
mkPrune s = LF $ Prune (read s)

mkMin s = LF $ Start (read s)

mkPrefix = MF . Prefix . fromMaybe "TODO:"

mkExecute = MF . Execute . fromMaybe "echo %n %d"
