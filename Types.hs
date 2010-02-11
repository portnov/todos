{-# LANGUAGE UnicodeSyntax, DeriveDataTypeable, TypeSynonymInstances, FlexibleInstances, NoMonomorphismRestriction #-}

module Types where

import Prelude hiding (putStr)
import System.IO.UTF8

import System.Console.ANSI

import Data.Function 
import Data.Generics hiding (GT)
import Data.Tree
import Data.List
import qualified Data.Map as M
import Text.ParserCombinators.Parsec

import Unicode

data TodoItem = Item {
    itemLevel ∷ ℤ,
    itemName ∷ String,
    itemTags ∷ [String],
    depends ∷ [String],
    itemStatus ∷ String,
    itemDescr ∷ String,
    fileName ∷ FilePath,
    lineNr ∷ Line}
    deriving (Eq,Data,Typeable)

type Todo = Tree TodoItem

type TodoMap = M.Map String Todo

newtype IOList = IOL [IO ()]

noIO = IOL []

class IOAdd a where
  (<++>) :: IOList -> a -> IOList

instance IOAdd (IO ()) where
  (IOL lst) <++> io = IOL (lst ++ [io])

instance IOAdd String where
  iol <++> s = iol <++> (putStr s)

runIOL ::  IOList -> IO ()
runIOL (IOL lst) = sequence_ lst

concatIOL ::  [IOList] -> IOList
concatIOL iols = IOL $ concat [lst | IOL lst <- iols]

intercalateIOL :: IO () -> [IOList] -> IOList
intercalateIOL s = concatIOL . intersperse (IOL [s])

intersperseIOL ::  IO () -> IOList -> IOList
intersperseIOL a (IOL lst) = IOL $ intersperse a lst

class ShowIO s where
  showIOL :: s -> IOList

instance ShowIO String where
  showIOL s = IOL [putStr s]

instance ShowIO (IO ()) where
  showIOL i = IOL [i]
  
showIO ::  (ShowIO a) => a -> IO ()
showIO = runIOL . showIOL

instance (Ord a) => Ord (Tree a) where
  compare = compare `on` rootLabel

data Limit = Unlimited
           | Limit ℤ
  deriving (Eq,Show)

instance Ord Limit where
    compare Unlimited Unlimited = EQ
    compare Unlimited _ = GT
    compare _ Unlimited = LT
    compare (Limit x) (Limit y) = compare x y

data CmdLineFlag = QF {queryFlag :: QueryFlag}
                 | MF {modeFlag :: ModeFlag}
                 | LF {limFlag :: LimitFlag}
                 | HelpF
    deriving (Eq,Show)

data QueryFlag = Tag String
               | Name {unName ∷ String}
               | Status String
               | AndCons
               | OrCons
               | NotCons
               | NoFilter
     deriving (Eq,Ord,Show)        

data LimitFlag = Prune {unPrune ∷ ℤ}
               | Start {unMin ∷ ℤ}
    deriving (Eq,Show)

data ModeFlag = OnlyFirst
              | Execute {unExecute ∷ String}
              | Prefix {unPrefix ∷ String}
              | Describe {unDescribe :: String}
    deriving (Eq,Ord,Show)

data Options = O [QueryFlag] [ModeFlag] [LimitFlag]
             | Help

data Query = Query {
               pruneL ∷ Limit,
               minL   ∷ Limit,
               query  ∷ Composed,
               showOnlyFirst ∷ 𝔹,
               commandToRun ∷ Maybe String,
               prefix ∷ Maybe String,
               descrFormat :: String}
    deriving (Eq,Show)

data Composed = Pred QueryFlag
              | And Composed Composed
              | Or Composed Composed
              | Not Composed
              | Empty
              | HelpC
    deriving (Eq,Show)

instance Show TodoItem where
    show item = s ⧺ " " ⧺ tags ⧺ name ⧺ (if null descr then "" else "    "⧺descr)
      where
        n = itemLevel item
        name = itemName item
        ts = itemTags item
        s = itemStatus item
        descr = itemDescr item
        tags = if null ts
                 then ""
                 else "[" ⧺ (unwords ts) ⧺ "] "

bold s = do
  setSGR [SetConsoleIntensity BoldIntensity]
  putStr s
  setSGR []

instance ShowIO TodoItem where
    showIOL item = noIO <++> s <++> " " <++> tags <++> bold name <++> (if null descr then "" else "    "⧺descr)
      where
        n = itemLevel item
        name = itemName item
        ts = itemTags item
        s = itemStatus item
        descr = itemDescr item
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

