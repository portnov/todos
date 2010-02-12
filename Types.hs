{-# LANGUAGE UnicodeSyntax, DeriveDataTypeable, TypeSynonymInstances, FlexibleInstances, NoMonomorphismRestriction #-}

module Types where

import Prelude hiding (putStr)
import System.IO.UTF8

import System.Console.ANSI

import Control.Monad.Reader
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
               | Status String
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
    deriving (Eq,Ord,Show)

data OutFlag = OnlyFirst 
             | Colors
    deriving (Eq,Ord,Show)

data OutConfig = OutConfig {
  outOnlyFirst ∷ Bool,
  outColors ∷ Bool }

type ConfigIO = ReaderT OutConfig IO ()

newtype IOList = IOL [ConfigIO]

noIO = IOL []

class IOAdd a where
  (<++>) ∷ IOList → a → IOList

instance IOAdd (IO ()) where
  (IOL lst) <++> io = IOL (lst ++ [lift io])

instance IOAdd ConfigIO where
  (IOL lst) <++> cio = IOL (lst ++ [cio])

instance IOAdd String where
  iol <++> s = iol <++> (putStr s)

runIOL ∷ OutConfig → IOList → IO ()
runIOL conf (IOL lst) = runReaderT (sequence_ lst) conf

concatIOL ∷  [IOList] → IOList
concatIOL iols = IOL $ concat [lst | IOL lst ← iols]

intercalateIOL ∷ IO () → [IOList] → IOList
intercalateIOL s = concatIOL ∘ intersperse (IOL [lift s])

intersperseIOL ∷  IO () → IOList → IOList
intersperseIOL a (IOL lst) = IOL $ intersperse (lift a) lst

class ShowIO s where
  showIOL ∷ s → IOList

instance ShowIO String where
  showIOL s = IOL [lift $ putStr s]

instance ShowIO (IO ()) where
  showIOL i = IOL [lift i]

instance ShowIO ConfigIO where
  showIOL i = IOL [i]
  
showIO ∷ (ShowIO a) ⇒ OutConfig → a → IO ()
showIO conf = (runIOL conf) ∘ showIOL

instance (Ord a) ⇒ Ord (Tree a) where
  compare = compare `on` rootLabel

-- TODO: - rename Options → QueryOptions or similar
data Options = O [QueryFlag] [ModeFlag] [OutFlag] [LimitFlag]
             | Help

data Query = Query {
               pruneL ∷ Limit,
               minL   ∷ Limit,
               query  ∷ Composed,
               showOnlyFirst ∷ 𝔹,
               commandToRun ∷ Maybe String,
               prefix ∷ Maybe String,
               descrFormat ∷ String}
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

setBold = lift $ setSGR [SetConsoleIntensity BoldIntensity]
setColor clr = lift $ setSGR [SetColor Foreground Dull clr]
reset = lift $ setSGR []

bold ∷ String → ConfigIO
bold s = do
  col ← asks outColors 
  when col setBold
  lift $ putStr s
  when col reset

lookupC k [] = Nothing
lookupC k ((lst,c):other) | k `elem` lst = Just c
                          | otherwise    = lookupC k other

statusColors = 
  [(["FIXED", "DONE"], Green),
   (["INVALID"],       Magenta),
   (["*"],             Red),
   (["?"],             Blue)]

colorStatus ∷ String → ConfigIO
colorStatus st =
  case lookupC st statusColors of
    Nothing → lift $ putStr st
    Just clr → do
      col ← asks outColors 
      when col $ setColor clr
      lift $ putStr st
      when col reset

instance ShowIO TodoItem where
    showIOL item = noIO <++> (colorStatus s) <++> " " <++> tags <++> bold name <++> (if null descr then "" else "    "⧺descr)
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

