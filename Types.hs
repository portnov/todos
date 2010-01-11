{-# LANGUAGE UnicodeSyntax, DeriveDataTypeable #-}

module Types where

import Data.Function 
import Data.Generics hiding (GT)
import Data.Tree
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

data Flag = Tag String
          | Name String
          | Status String
          | Prune ℤ
          | Start ℤ
          | AndCons
          | OrCons
          | NotCons
          | NoFilter
          | OnlyFirst
          | HelpF
     deriving (Eq,Ord,Show)         

data Query = Query { pruneL :: Limit, minL :: Limit, query :: Composed }
           | Help
    deriving (Eq,Show)

data Composed = Pred Flag
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
