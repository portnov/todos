{-# LANGUAGE UnicodeSyntax, PatternGuards #-}

module CmdLine where

import Prelude hiding (putStrLn,print)
import Codec.Binary.UTF8.String
import System.IO.UTF8
import System (getArgs)
import System.Console.GetOpt
import Data.Maybe

import Unicode
import Types
import TodoTree
import ConstrSet 

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

appendF (O q m l) (QF f) = O (f:q) m l
appendF (O q m l) (MF f) = O q (f `insert` m) l
appendF (O q m l) (LF f) = O q m (f `insert` l)
appendF _ HelpF = Help

parseFlags :: [CmdLineFlag] -> Options
parseFlags lst | HelpF `elem` lst = Help
parseFlags [] = O [] empty empty
parseFlags (f:fs) = (parseFlags fs) `appendF` f

buildQuery :: Options -> Query
buildQuery (O qflags mflags lflags) = Query limitP limitM composedFlags onlyFirst command aprefix dformat
  where
    composedFlags = parseQuery qflags
    (limitP,limitM) = parseLimits (toList lflags)

    onlyFirst = OnlyFirst `elemC` mflags
    command = unExecute `fmap` (mflags `selectByConstrOf` (Execute undefined))

    aprefix = unPrefix `fmap` (mflags `selectByConstrOf` (Prefix undefined))

    dformat = unDescribe $ selectByConstrOf' (Describe "%d") mflags (Describe undefined)

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

-- TODO: * refactor
parseQuery ∷ [QueryFlag] → Composed
parseQuery flags = foldl appendC Empty flags

parseCmdLine ∷  String -> String -> IO (Options, [FilePath])
parseCmdLine defPrefix defExec = do
  args ← getArgs
  return $ case getOpt RequireOrder (options defPrefix defExec) (map decodeString args) of
        (flags, [],      [])     → (parseFlags flags, ["TODO"])
        (flags, nonOpts, [])     → (parseFlags flags, nonOpts)
        (_,     _,       msgs)   → error $ concat msgs ⧺ usage

usage ∷  String
usage = usageInfo header (options "" "")
  where 
    header = "Usage: todos [OPTION...] [INPUT FILES]"

options ∷  String -> String -> [OptDescr CmdLineFlag]
options defPrefix defExec = [
    Option "1" ["only-first"] (NoArg (MF OnlyFirst))    "show only first matching entry",
    Option "A" ["prefix"] (OptArg (mkPrefix defPrefix) "PREFIX") "use alternate parser: read only lines starting with PREFIX",
    Option "D" ["describe"] (OptArg mkDescribe "FORMAT") "use FORMAT for descriptions",
    Option "p" ["prune"]  (ReqArg mkPrune "N")     "limit tree height to N",
    Option "m" ["min-depth"] (ReqArg mkMin "N")    "show first N levels of tree unconditionally",
    Option "t" ["tag"]    (ReqArg mkTag "TAG")       "find items marked with TAG",
    Option "g" ["grep"]   (ReqArg mkName "PATTERN")  "find items with PATTERN in name",
    Option "s" ["status"] (ReqArg mkStatus "STRING") "find items with status equal to STRING",
    Option "a" ["and"]    (NoArg (QF AndCons))          "logical AND",
    Option "o" ["or"]     (NoArg (QF OrCons))           "logical OR",
    Option "n" ["not"]    (NoArg (QF NotCons))          "logical NOT",
    Option "e" ["exec"]   (OptArg (mkExecute defExec) "COMMAND") "run COMMAND on each matching entry",
    Option "h" ["help"]   (NoArg HelpF)            "display this help"
  ]

mkTag t = QF $ Tag t

mkName n = QF $ Name n

mkStatus s = QF $ Status s

mkDescribe Nothing = MF $ Describe "%d"
mkDescribe (Just f) = MF $ Describe f

mkPrune ∷  String → CmdLineFlag
mkPrune s = LF $ Prune (read s)

mkMin s = LF $ Start (read s)

mkPrefix "" = MF . Prefix . fromMaybe "TODO:"
mkPrefix def = MF . Prefix . fromMaybe def

mkExecute def = MF . Execute . fromMaybe def
