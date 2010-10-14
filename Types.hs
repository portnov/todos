{-# LANGUAGE UnicodeSyntax, DeriveDataTypeable, TypeSynonymInstances, FlexibleInstances, NoMonomorphismRestriction #-}

module Types where

import Prelude hiding (putStr, putStrLn,readFile,getContents,print)
import IO
import System.Console.ANSI
import Data.Hash

import Control.Monad.Reader
import Data.Function 
import Data.Generics hiding (GT)
import Data.Char (toUpper)
import Data.Maybe
import Data.Tree
import Data.List
import qualified Data.Map as M
import Text.ParserCombinators.Parsec
import Numeric

import Unicode

data DateType = StartDate
              | EndDate
              | Deadline
  deriving (Eq)

instance Show DateType where
  show StartDate = "start"
  show EndDate = "end"
  show Deadline = "deadline"

data DateTime =
  DateTime {
    year ∷ Int,
    month ∷ Int,
    day ∷ Int,
    hour ∷ Int,
    minute ∷ Int,
    second ∷ Int }
  deriving (Eq,Ord,Data,Typeable)

months ∷ [String]
months = ["january",
          "february",
          "march",
          "april",
          "may",
          "june",
          "july",
          "august",
          "september",
          "october",
          "november",
          "december"]

capitalize ∷ String → String
capitalize [] = []
capitalize (x:xs) = (toUpper x):xs

showMonth ∷  Int → String
showMonth i = capitalize $ months !! (i-1)

instance Show DateTime where
  show (DateTime y m d h min s) = 
    show d ⧺ " " ⧺ showMonth m ⧺ " " ⧺ show y ⧺ ", " ⧺
      show h ⧺ ":" ⧺ show min ⧺ ":" ⧺ show s

data Time = 
  Time {
    tHour ∷ Int,
    tMinute ∷ Int,
    tSecond ∷ Int }
  deriving (Eq,Ord,Show,Data,Typeable)

data TodoItem = Item {
    itemLevel ∷ ℤ,
    itemName ∷ String,
    itemTags ∷ [String],
    depends ∷ [String],
    itemStatus ∷ String,
    itemDescr ∷ String,
    startDate ∷ Maybe DateTime,
    endDate ∷ Maybe DateTime,
    deadline ∷ Maybe DateTime,
    fileName ∷ FilePath,
    lineNr ∷ Line}
  deriving (Eq,Data,Typeable)

instance Hashable TodoItem where
    hash item = foldl1 combine $ map ($ item) [hash ∘ itemName, hash ∘ itemDescr,
                                               hash ∘ itemTags, hash ∘ itemStatus]

makeId :: TodoItem → String
makeId item =
  let s = showHex (asWord64 $ hash item) ""
      l = length s 
  in  if l < 16 
        then replicate (16-l) '0' ++ s
        else s

type Todo = Tree TodoItem

type TodoMap = M.Map String Todo

data Limit = Unlimited
           | Limit ℤ
  deriving (Eq,Show)

instance Ord Limit where
    compare Unlimited Unlimited = EQ
    compare Unlimited _ = GT
    compare _ Unlimited = LT
    compare (Limit x) (Limit y) = compare x y

data CmdLineFlag = QF {queryFlag ∷ QueryFlag}
                 | MF {modeFlag ∷ ModeFlag}
                 | OF {outFlag ∷ OutFlag}
                 | LF {limFlag ∷ LimitFlag}
                 | HelpF
    deriving (Eq,Show)

data QueryFlag = Tag String
               | Name {unName ∷ String}
               | IdIs String
               | Status String
               | Description String
               | StartDateIs DateTime
               | EndDateIs DateTime
               | DeadlineIs DateTime
               | AndCons
               | OrCons
               | NotCons
               | NoFilter
     deriving (Eq,Ord,Show)        

data LimitFlag = Prune {unPrune ∷ ℤ}
               | Start {unMin ∷ ℤ}
    deriving (Eq,Show)

data ModeFlag = Execute {unExecute ∷ String}
              | Prefix {unPrefix ∷ String}
              | Describe {unDescribe ∷ String}
              | DoNotReadStatus
              | SetStatus {newStatus ∷ String}
    deriving (Eq,Ord,Show)

data OutFlag = OnlyFirst 
             | Colors
             | Ids
             | DotExport
             | Sort {getSorting ∷ SortingType}
    deriving (Eq,Ord,Show)

data SortingType = DoNotSort
                 | ByTitle
                 | ByStatus
                 | ByTags 
                 | ByStartDate
                 | ByEndDate
                 | ByDeadline
    deriving (Eq,Ord,Show)

readSort ∷ String → SortingType
readSort "no" = DoNotSort
readSort "title" = ByTitle
readSort "status" = ByStatus
readSort "tags" = ByTags
readSort "start-date" = ByStartDate
readSort "end-date" = ByEndDate
readSort "deadline" = ByDeadline
readSort s = error $ "Unknown sorting type: "++s

type Transformer = Reader Config (Todo → [Todo])
type ListTransformer = Reader Config ([Todo] → [Todo])

transformList ∷  r → Reader r (t → a) → t → a
transformList conf tr list = do
    f ← tr
    return (f list)
  `runReader` conf

data OutItem = OutString String
             | OutSetColor Color
             | SetBold
             | ResetAll
    deriving (Show)

type Formatter = Reader Config [OutItem]

newtype IOList = IOL [Formatter]

startFormat ∷ Formatter
startFormat = return []

outString ∷ String → Formatter
outString s = return [OutString s]

newLine ∷ Formatter
newLine = outString "\n"

class ConfigAdd a where
  (<++>) ∷ Formatter → a → Formatter

instance ConfigAdd Formatter where
  (<++>) = liftM2 (⧺)

instance ConfigAdd String where
  cm <++> s = cm <++> ((return [OutString s]) ∷ Formatter)

setBold ∷  IO ()
setBold = setSGR [SetConsoleIntensity BoldIntensity]

setColor ∷  Color → IO ()
setColor clr = setSGR [SetColor Foreground Dull clr]

reset ∷  IO ()
reset = setSGR []

outItem ∷  OutItem → IO ()
outItem (OutString s)   = putStr s
outItem (OutSetColor c) = setColor c
outItem SetBold         = setBold
outItem ResetAll        = reset

runFormatter ∷ Config → Formatter → IO ()
runFormatter conf cm = 
  let lst = runReader cm conf
  in  mapM_ outItem lst

class ConfigShow s where
  configShow ∷ s → Formatter

instance ConfigShow String where
  configShow s = return [OutString s]

instance ConfigShow Formatter where
  configShow = id
  
showIO ∷ (ConfigShow a) ⇒ Config → a → IO ()
showIO conf = (runFormatter conf) ∘ configShow

instance (Ord a) ⇒ Ord (Tree a) where
  compare = compare `on` rootLabel

-- TODO: - rename Options → QueryOptions or similar
data Options = O [QueryFlag] [ModeFlag] [OutFlag] [LimitFlag]
             | Help

data TodoCommand =
    JustShow
  | ShowAsDot
  | SystemCommand String
  deriving (Eq, Show)

data Config = Config {
      outOnlyFirst ∷ 𝔹,
      outColors ∷ 𝔹,
      outIds :: 𝔹,
      sorting ∷ SortingType,
      pruneL ∷ Limit,
      minL   ∷ Limit,
      commandToRun ∷ TodoCommand,
      prefix ∷ Maybe String,
      descrFormat ∷ String,
      skipStatus ∷ 𝔹,
      forcedStatus ∷ Maybe String,
      query ∷ Composed }
    deriving (Eq,Show)

emptyConfig = Config {
  outOnlyFirst = False,
  outColors = False,
  outIds = False,
  sorting = DoNotSort,
  pruneL = Unlimited,
  minL = Unlimited,
  commandToRun = JustShow,
  prefix = Nothing,
  descrFormat = "%d",
  skipStatus = False,
  forcedStatus = Nothing,
  query = Empty }

data Composed = Pred QueryFlag
              | And Composed Composed
              | Or Composed Composed
              | Not Composed
              | Empty
              | HelpC
    deriving (Eq,Show)

is ∷  (Functor f) ⇒ t → f a → f (t, a)
t `is`  x = (\a → (t,a)) `fmap` x

showDate ∷  (Show t, Show t1) ⇒ (t, t1) → [Char]
showDate (t,d) = show t ⧺ ": " ⧺ show d

showDates ∷  (Show t, Show t1) ⇒ [Maybe (t, t1)] → [Char]
showDates = intercalate "; " ∘ map showDate ∘ catMaybes

instance Show TodoItem where
    show item = s ⧺ " " ⧺ dates ⧺ tags ⧺ name ⧺ (if null descr then "" else "    "⧺descr)
      where
        n = itemLevel item
        name = itemName item
        ts = itemTags item
        s = itemStatus item
        descr = itemDescr item
        dates | null dates' = ""
              | otherwise = "(" ⧺ dates' ⧺ ") "
        dates' = showDates [StartDate `is` startDate item, EndDate `is` endDate item, Deadline `is` deadline item]
        tags = if null ts
                 then ""
                 else "[" ⧺ (unwords ts) ⧺ "] "

bold ∷ String → Formatter
bold s = do
  col ← asks outColors 
  if col
    then return [SetBold, OutString s, ResetAll]
    else return [OutString s]

lookupC ∷  (Eq a1) ⇒ a1 → [([a1], a)] → Maybe a
lookupC k [] = Nothing
lookupC k ((lst,c):other) | k ∈ lst   = Just c
                          | otherwise = lookupC k other

statusColors ∷  [([String], Color)]
statusColors = 
  [(["FIXED", "DONE"], Green),
   (["INVALID"],       Magenta),
   (["*"],             Red),
   (["?"],             Blue)]

colorStatus ∷ String → Formatter
colorStatus st =
  case lookupC st statusColors of
    Nothing → return [OutString st]
    Just clr → do
      col ← asks outColors 
      if col
        then return [OutSetColor clr, OutString st, ResetAll]
        else return [OutString st]

instance ConfigShow TodoItem where
    configShow item = startFormat <++> colorStatus s <++> " " <++> dates <++> tags <++> bold name <++> (if null descr then "" else "    "⧺descr)
      where
        n = itemLevel item
        name = itemName item
        ts = itemTags item
        s = itemStatus item
        descr = itemDescr item
        dates | null dates' = ""
              | otherwise = "(" ⧺ dates' ⧺ ") "
        dates' = showDates [StartDate `is` startDate item, EndDate `is` endDate item, Deadline `is` deadline item]
        tags = if null ts
                 then ""
                 else "[" ⧺ (unwords ts) ⧺ "] "

instance Ord TodoItem where
  compare item1 item2 = 
      let c1 = (compare `on` itemLevel) item1 item2
          c2 = (compare `on` itemStatus) item1 item2
          c3 = (compare `on` itemName) item1 item2
      in  if c1 == EQ
            then if c2 == EQ 
                   then c3
                   else c2
            else c1

type TParser a = GenParser Char Config a

