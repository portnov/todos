{-# LANGUAGE UnicodeSyntax, DeriveDataTypeable, TypeSynonymInstances, FlexibleInstances, NoMonomorphismRestriction #-}

module Todos.Types where

import Prelude hiding (putStr, putStrLn,readFile,getContents,print)
import Prelude.Unicode
import Data.Hash

import Data.Function 
import Data.Generics hiding (GT)
import Data.Char (toUpper)
import Data.Maybe
import Data.Tree
import Data.List
import Data.Dates
import qualified Data.Map as M
import Text.Parsec
import Numeric

type 𝔹 = Bool

-- | Kind of date
data DateType = StartDate
              | EndDate
              | Deadline
  deriving (Eq)

instance Show DateType where
  show StartDate = "start"
  show EndDate = "end"
  show Deadline = "deadline"

-- | capitalize first letter of the string
capitalize ∷ String → String
capitalize [] = []
capitalize (x:xs) = (toUpper x):xs

-- | TODO item itself.
data TodoItem = Item {
    itemLevel ∷ ℤ,               -- ^ Indentation level (from source file)
    itemPrefix ∷ String,         -- ^ A prefix before item in source file (or empty string)
    itemName ∷ String,           -- ^ Name (title) of the item
    itemTags ∷ [String],         -- ^ Tags of the item
    depends ∷ [String],          -- ^ Names (titles) of item's depends
    itemStatus ∷ String,         -- ^ Status of the item
    itemDescr ∷ String,          -- ^ Description of the item
    startDate ∷ Maybe DateTime,  -- ^ Date when TODO is planned to start
    endDate ∷ Maybe DateTime,    -- ^ Date when TODO is planned to end
    deadline ∷ Maybe DateTime,   -- ^ Deadline for this TODO
    fileName ∷ FilePath,         -- ^ Path to the source file
    lineNr ∷ Line,               -- ^ Line in the source file, where this item was defined
    itemNumber ∷ ℤ               -- ^ Raw number of item
  }
  deriving (Eq,Data,Typeable)

instance Hashable TodoItem where
    hash item = foldl1 combine $ map ($ item) [hash ∘ itemName, hash ∘ itemDescr,
                                               hash ∘ itemTags, hash ∘ itemStatus]

-- | Make an ID for any hashable item. 16 hexadecimal digits.
makeId :: (Hashable a) ⇒ a → String
makeId item =
  let s = showHex (asWord64 $ hash item) ""
      l = length s 
  in  if l < 16 
        then replicate (16-l) '0' ++ s
        else s

-- | Tree of TODO items.
type Todo = Tree TodoItem

type TodoMap = M.Map String Todo

data Limit = Unlimited
           | Limit {unLimit ∷ ℤ}
  deriving (Eq,Show)

instance Ord Limit where
    compare Unlimited Unlimited = EQ
    compare Unlimited _ = GT
    compare _ Unlimited = LT
    compare (Limit x) (Limit y) = compare x y

-- | Command line flag
data CmdLineFlag = QF {queryFlag ∷ QueryFlag}
                 | MF {modeFlag ∷ ModeFlag}
                 | OF {outFlag ∷ OutFlag}
                 | LF {limFlag ∷ LimitFlag}
                 | HelpF
    deriving (Eq,Show)

-- | Flags to specify query
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

-- | Flags to specify parsing mode
data ModeFlag = Execute {unExecute ∷ String}
              | Prefix {unPrefix ∷ String}
              | Format {getFormat ∷ String}
              | DoNotReadStatus
              | SetStatus {newStatus ∷ String}
              | SetTopStatus {newTopStatus ∷ String}
              | GroupByFile
              | GroupByTag
              | GroupByStatus
    deriving (Eq,Ord,Show)

-- | Flags to control output
data OutFlag = OnlyFirst 
             | Colors
             | Highlight
             | Ids
             | DotExport
             | IndentWith {getIndentString ∷ String}
             | Sort {getSorting ∷ SortingType}
    deriving (Eq,Ord,Show)

-- | Type of sorting
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

instance (Ord a) ⇒ Ord (Tree a) where
  compare = compare `on` rootLabel

-- TODO: - rename Options → QueryOptions or similar
-- | Result of parsing command line
data Options = O [QueryFlag] [ModeFlag] [OutFlag] [LimitFlag]
             | Help

-- | What to do with selected items
data TodoCommand =
    JustShow              -- ^ Just output items to console
  | ShowAsDot             -- ^ Output graph in DOT format
  | SystemCommand String  -- ^ Execute this system command for each item
  deriving (Eq, Show)

-- | Data type to store complex queries
data Composed = Pred QueryFlag            -- ^ Simple query
              | And Composed Composed     -- ^ Logical AND
              | Or Composed Composed      -- ^ Logical OR
              | Not Composed              -- ^ Logical NOT
              | Empty                     -- ^ Empty query
              | HelpC                     -- ^ User requests help
    deriving (Eq,Show)

is ∷  (Functor f) ⇒ t → f a → f (t, a)
t `is`  x = (\a → (t,a)) `fmap` x

showDate ∷  (DateType, DateTime) → String
showDate (t,d) = show t ⧺ ": " ⧺ show d

showDates ∷  [Maybe (DateType, DateTime)] → String
showDates = intercalate "; " ∘ map showDate ∘ catMaybes

instance Show TodoItem where
    show item = s ⧺ " " ⧺ dates ⧺ tags ⧺ name ⧺ (if null descr then "" else "    "⧺descr)
      where
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

