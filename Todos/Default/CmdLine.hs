{-# LANGUAGE UnicodeSyntax, PatternGuards #-}

-- | Module for parsing command line options and build queries. These functions
-- are used by default, but user can supply his own functions.
module Todos.Default.CmdLine
  (parseCmdLine',
   glob,
   buildQuery,
   compose,
   usage)
  where

import Prelude hiding (putStrLn,readFile,getContents,print)
import Prelude.Unicode
import System.Console.GetOpt
import System.FilePath.Glob hiding (glob)
import Data.Maybe
import Data.List (sort)
import Data.Dates

import Todos.IO
import Todos.Types
import Todos.Tree
import Todos.Default.Config

-- | Compose predicate from Composed
compose ∷ DateTime       -- ^ Current date/time
        → Composed       -- ^ Composed query
        → (TodoItem → 𝔹)
compose _ Empty             = const True
compose _ (Pred NoFilter)   = const True
compose _ (Pred (Tag s))    = tagPred s
compose _ (Pred (Name s))   = grepPred s
compose _ (Pred (Description s))   = descPred s
compose _ (Pred (Status s)) = statusPred s
compose _ (Pred (IdIs s)) = idPred s
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

appendF ∷ Options → CmdLineFlag → Options
appendF (O q m o l) (QF f) = O (f:q) m o l
appendF (O q m o l) (MF f) = O q (f:m) o l
appendF (O q m o l) (OF f) = O q m (f:o) l
appendF (O q m o l) (LF f) = O q m o (f:l)
appendF _ HelpF = Help
appendF Help _  = Help

parseFlags ∷ [CmdLineFlag] → Options
parseFlags lst | HelpF ∈ lst = Help
parseFlags [] = O [] [] [] []
parseFlags (f:fs) = (parseFlags fs) `appendF` f

-- | Build DefaultConfig (with query etc) from Options
buildQuery ∷ BaseConfig    -- ^ Default config
           → Options       -- ^ Cmdline options
           → DefaultConfig
buildQuery _ Help = error "Internal error: buildQuery does no sense for Help!"
buildQuery dc (O qflags mflags oflags lflags) =
    DConfig {
      baseConfig = BConfig {
          outOnlyFirst = update outOnlyFirst onlyFirst,
          outColors    = update outColors    colors,
          outIds       = update outIds       showIds,
          outHighlight = update outHighlight highlight,
          sorting      = update sorting      srt,
          pruneL       = update pruneL       limitP,
          minL         = update minL         limitM,
          commandToRun = update commandToRun command,
          prefix       = update prefix       aprefix,
          outputFormat = update outputFormat dformat,
          indentString = update indentString indent,
          skipStatus   = update skipStatus   noStatus,
          groupByFile  = update groupByFile  doGroupByFile,
          groupByTag   = update groupByTag   doGroupByTag,
          groupByStatus = update groupByStatus doGroupByStatus,
          forcedStatus = update forcedStatus setStatus,
          topStatus    = update topStatus    setTopStatus },
      query        = fromMaybe Empty composedFlags }
  where
    update fn Nothing  = fn dc
    update _  (Just x) = x

    x ? lst | x ∈ lst   = Just True
            | otherwise = Nothing

    composedFlags | null qflags = Nothing
                  | otherwise   = Just $ parseQuery qflags
    (limitP,limitM) | null lflags = (Nothing, Nothing)
                    | otherwise   = parseLimits (unLimit $ pruneL dc) (unLimit $ minL dc) lflags

    onlyFirst = OnlyFirst ? oflags
    colors    = Colors    ? oflags
    highlight = Highlight ? oflags
    showIds   = Ids       ? oflags

    srtFlags = filter isSort oflags
    srt | null srtFlags = Nothing
        | otherwise     = Just $ getSorting (last srtFlags)

    doGroupByFile   = GroupByFile   ? mflags
    doGroupByTag    = GroupByTag    ? mflags
    doGroupByStatus = GroupByStatus ? mflags

    cmdFlags  = filter isCommand mflags
    command | DotExport ∈ oflags = Just $ ShowAsDot
            | null cmdFlags      = Nothing
            | otherwise          = Just $ SystemCommand $ unExecute (last cmdFlags)

    prefixFlags = filter isPrefix mflags
    aprefix | null prefixFlags = Nothing
            | otherwise        = Just $ Just $ unPrefix (last prefixFlags)

    dflags = filter isFormat mflags
    dformat | null dflags = Nothing
            | otherwise   = Just $ getFormat $ last dflags

    iflags = filter isIndent oflags
    indent | null iflags = Nothing
           | otherwise   = Just $ getIndentString $ last iflags

    noStatus = DoNotReadStatus ? mflags
    newStatusFlags = filter isSetStatus mflags
    setStatus | null newStatusFlags = Nothing
              | otherwise           = Just $ Just $ newStatus $ last newStatusFlags

    topStatusFlags = filter isTopStatus mflags
    setTopStatus | null topStatusFlags = Nothing
                 | otherwise           = Just $ Just $ newTopStatus $ last topStatusFlags

    isSort (Sort _) = True
    isSort _        = False
    isFormat (Format _) = True
    isFormat _          = False
    isIndent (IndentWith _) = True
    isIndent _              = False
    isCommand (Execute _) = True
    isCommand _           = False
    isPrefix (Prefix _) = True
    isPrefix _          = False
    isSetStatus (SetStatus _)  = True
    isSetStatus _              = False
    isTopStatus (SetTopStatus _) = True
    isTopStatus _                = False

parseLimits ∷ ℤ → ℤ → [LimitFlag] → (Maybe Limit,Maybe Limit)
parseLimits dlp dlm flags = (Just limitP, Just limitM)
  where
    pruneFlags = filter isPrune flags
    minFlags   = filter isMin flags

    limitP'       = foldl min Unlimited $ map (Limit ∘ unPrune) pruneFlags
    limitP | Unlimited ← limitP' = Limit dlp
           | otherwise           = limitP'

    limitM'       = foldl max (Limit 0) $ map (Limit ∘ unMin) minFlags
    limitM | Unlimited ← limitM' = Limit dlm
           | otherwise           = limitM'

    isPrune (Prune _) = True
    isPrune _         = False

    isMin   (Start _) = True
    isMin   _         = False

parseQuery ∷ [QueryFlag] → Composed
parseQuery flags = foldl appendC Empty flags

-- | Parse command line
parseCmdLine' ∷ DateTime             -- ^ Current date/time
             → [String]              -- ^ Command line args
             → Either String (Options, [FilePath]) -- ^ Error message or (Options, list of files)
parseCmdLine' currDate args = 
  case getOpt Permute (options currDate) (map ensureUnicode args) of
        (flags, [],      [])     → Right (parseFlags flags, ["TODO"])
        (flags, nonOpts, [])     → Right (parseFlags flags, nonOpts)
        (_,     _,       msgs)   → Left $ concat msgs ⧺ usage

isPattern ∷ String → 𝔹
isPattern s = ('*' ∈ s) || ('?' ∈ s)

-- | For given list of glob masks, return list of matching files
glob ∷ [FilePath] → IO [FilePath]
glob list = do
  let patterns = filter isPattern list
      files = filter (not ∘ isPattern) list
  matches ← globDir (map compile patterns) "." 
  return $ sort $ files ⧺ concat matches

-- | Usage help for default command line options
usage ∷  String
usage = usageInfo header (options undefined)
  where 
    header = "Usage: todos [OPTION...] [INPUT FILES]"

options ∷ DateTime → [OptDescr CmdLineFlag]
options currDate = [
    Option "1" ["only-first"] (NoArg (OF OnlyFirst))                 "show only first matching entry",
    Option "c" ["color"]      (NoArg (OF Colors))                    "show colored output",
    Option "H" ["highlight"]  (NoArg (OF Highlight))                 "instead of filtering TODOs, just highlight matching the query",
    Option "I" ["show-ids"]   (NoArg (OF Ids))                       "show IDs of todos",
    Option "A" ["prefix"]     (OptArg mkPrefix "PREFIX")             "use alternate parser: read only lines starting with PREFIX",
    Option ""  ["dot"]        (NoArg (OF DotExport))                 "output entries in DOT (graphviz) format",
    Option "D" ["format"]     (ReqArg mkFormat "FORMAT")             "use FORMAT to format items",
    Option "k" ["indent-with"] (OptArg mkIndent "STRING")            "use STRING instead of two spaces for items indentation",
    Option "w" ["no-status"]  (NoArg (MF DoNotReadStatus))           "do not read status field from TODOs",
    Option ""  ["set-status"] (ReqArg mkSetStatus "STRING")          "force all TODOs status to be equal to STRING",
    Option ""  ["set-root-status"] (ReqArg mkTopStatus "STRING")     "force statuses of root TODOs to be equal to STRING",
    Option "F" ["by-file"]    (NoArg (MF GroupByFile))               "group TODOs by source file",
    Option "T" ["by-tag"]     (NoArg (MF GroupByTag))                "group TODOs by tag",
    Option "Z" ["by-status"]  (NoArg (MF GroupByStatus))             "group TODOs by status",
    Option "p" ["prune"]      (ReqArg mkPrune "N")                   "limit tree height to N",
    Option "m" ["min-depth"]  (ReqArg mkMin "N")                     "show first N levels of tree unconditionally",
    Option "t" ["tag"]        (ReqArg mkTag "TAG")                   "find items marked with TAG",
    Option "g" ["grep"]       (ReqArg mkName "PATTERN")              "find items with PATTERN in name",
    Option "G" ["description"] (ReqArg mkDescr "PATTERN")            "find items with PATTERN in description",
    Option "s" ["status"]     (ReqArg mkStatus "STRING")             "find items with status equal to STRING",
    Option "i" ["id"]         (ReqArg mkIdQ "STRING")                "find items with ID equal to STRING",
    Option "a" ["and"]        (NoArg (QF AndCons))                   "logical AND",
    Option "o" ["or"]         (NoArg (QF OrCons))                    "logical OR",
    Option "n" ["not"]        (NoArg (QF NotCons))                   "logical NOT",
    Option ""  ["sort"]       (ReqArg mkSort "FIELD")                "specify sorting",
    Option "e" ["exec"]       (OptArg mkExecute "COMMAND")           "run COMMAND on each matching entry",
    Option "S" ["start-date"] (ReqArg (mkStartDate currDate) "DATE") "find items with start date bounded with DATE",
    Option "E" ["end-date"]   (ReqArg (mkEndDate currDate) "DATE")   "find items with end date bounded with DATE",
    Option "d" ["deadline"]   (ReqArg (mkDeadline currDate) "DATE")  "find items with deadline bounded with DATE",
    Option "h" ["help"]       (NoArg HelpF)                          "display this help"
  ]

mkSort ∷  String → CmdLineFlag
mkSort s = OF $ Sort $ readSort s

mkTag ∷  String → CmdLineFlag
mkTag t = QF $ Tag t

mkName ∷  String → CmdLineFlag
mkName n = QF $ Name n

mkStatus ∷  String → CmdLineFlag
mkStatus s = QF $ Status s

mkIdQ ∷  String → CmdLineFlag
mkIdQ s = QF $ IdIs s

mkDescr ∷  String → CmdLineFlag
mkDescr s = QF $ Description s

forceEither ∷  (Show t) ⇒ Either t b → b
forceEither (Right x) = x
forceEither (Left x) = error $ show x

mkStartDate ∷  DateTime → String → CmdLineFlag
mkStartDate dt s = QF $ StartDateIs $ forceEither $ parseDate dt s

mkEndDate ∷  DateTime → String → CmdLineFlag
mkEndDate dt s = QF $ EndDateIs $ forceEither $ parseDate dt s

mkDeadline ∷  DateTime → String → CmdLineFlag
mkDeadline dt s = QF $ DeadlineIs $ forceEither $ parseDate dt s

mkFormat ∷  String → CmdLineFlag
mkFormat f = MF $ Format f

mkIndent ∷ Maybe String → CmdLineFlag
mkIndent Nothing  = OF $ IndentWith ""
mkIndent (Just s) = OF $ IndentWith s

mkSetStatus ∷ String → CmdLineFlag
mkSetStatus st = MF $ SetStatus st

mkTopStatus ∷ String → CmdLineFlag
mkTopStatus st = MF $ SetTopStatus st

mkPrune ∷  String → CmdLineFlag
mkPrune s = LF $ Prune (read s)

mkMin ∷  String → CmdLineFlag
mkMin s = LF $ Start (read s)

mkPrefix ∷  Maybe [Char] → CmdLineFlag
mkPrefix = MF ∘ Prefix ∘ fromMaybe "TODO:"

mkExecute ∷  Maybe [Char] → CmdLineFlag
mkExecute = MF ∘ Execute ∘ fromMaybe "echo %n %d"
