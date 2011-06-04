{-# LANGUAGE UnicodeSyntax, NoMonomorphismRestriction, FlexibleInstances, TypeSynonymInstances #-}
module Todos.Tree 
  (delTag, addTag, mapTags,
   flattern,
   pruneSelector,
   tagPred, statusPred, grepPred, descPred, datePred, idPred,
   forT, mapT,
   treeLines, enumerateTodos, itemByNumber,
   spawn)
  where

import Prelude hiding (putStrLn,readFile,getContents,print)
import Control.Monad
import qualified Data.Traversable as T
import Data.Maybe
import Data.Generics
import Data.List
import Data.Tree
import Text.Regex.PCRE
import System.Cmd (system)

import Todos.Types
import Todos.Unicode
import Todos.Config
import Todos.CommandParser

mapTags ∷  ([String] → [String]) → [Todo] → [Todo]
mapTags f = map ⋄ everywhere (mkT changeTags :: Data a => a -> a)
  where
    changeTags ∷ TodoItem → TodoItem
    changeTags item@(Item {itemTags=ts}) = item {itemTags = f ts}
        
addTag ∷ String → [Todo] → [Todo]
addTag t = mapTags (t:)

delTag ∷ String → [Todo] → [Todo]
delTag t = mapTags (delete t)

pruneSelector ∷  BaseConfig → (TodoItem → 𝔹) → (Todo → [Todo])
pruneSelector bc pred =
  let Limit n = pruneL bc
      Limit m = minL   bc
  in  pruneSelector' n m pred
        
pruneSelector' ∷ ℤ → ℤ → (TodoItem → 𝔹) → (Todo → [Todo])
pruneSelector' n m pred = select n 0 False
    where
        select k t b (Node item trees) | t < m       = [Node item ⋄ concatMap (select (n-1) (t+1) True) trees]
                                       | pred item   = [Node item ⋄ concatMap (select (n-1) (t+1) True) trees]
                                       | (k > 0) ∧ b = [Node item ⋄ concatMap (select (k-1) (t+1) True) trees]
                                       | k > 0       = concatMap (select (k-1) (t+1) False) trees
                                       | otherwise   = []                                               

-- | Check if item has given tag
-- | Check if item has given tag
tagPred ∷  String → TodoItem → 𝔹
tagPred tag = \item → tag ∈ itemTags item

-- | Check if item has given status
statusPred ∷  String → TodoItem → 𝔹
statusPred st = \item → st == itemStatus item
        
-- | Check if item's title matches to given regexp
grepPred ∷ String → TodoItem → 𝔹
grepPred pattern = \item → itemName item =~ pattern

-- | Check if item's description matches to given regexp
descPred ∷ String → TodoItem → 𝔹
descPred pattern = \item → itemDescr item =~ pattern

-- | Check if item has given ID
idPred :: String → TodoItem → 𝔹
idPred hash = \item → makeId item == hash

isLT ∷  (Ord t) ⇒ Maybe t → t → 𝔹
isLT Nothing _ = False
isLT (Just x) y = x <= y

isGT ∷  (Ord t) ⇒ Maybe t → t → 𝔹
isGT Nothing _ = False
isGT (Just x) y = x >= y

datePred ∷  (Ord a) ⇒ (t → Maybe a) → a → a → t → 𝔹
datePred selector curr dt | dt >= curr = \item → selector item `isLT` dt
                          | otherwise  = \item → selector item `isGT` dt

-- | Flattern the tree
flattern ∷ [Todo] → [Todo]
flattern = concatMap flat
    where
        flat ∷ Todo → [Todo]
        flat (Node item trees) = (Node item []):(concatMap flat trees)

-- | For each item in the tree, execute given monadic action (this is similar
-- to forM, but for trees instead of lists).
forT ∷ (Monad m, Eq t) ⇒ [Tree t] → (t → m a) → m [b]
forT todos f = forM (nub todos) forT'
  where
    forT' (Node item trees) =
      do f item
         res ← forM trees forT'
         return $ last res

-- | Similar to map, but for trees instead of lists.
mapT ∷ (t → t) → [Tree t] → [Tree t]
mapT f todos = map mapT' todos
  where
    mapT' (Node item trees) = Node (f item) (mapT f trees)

treeLines ∷ [Tree t] → ℤ
treeLines todos = sum $ map treeLines' todos
  where
    treeLines' (Node _ children) = 1 + (sum $ map treeLines' children)
    
enumerateTodos ∷ [Todo] → [Todo]
enumerateTodos list = snd $ T.mapAccumL enumTree 1 list
  where
    enumTree ∷ ℤ → Todo → (ℤ, Todo)
    enumTree i tree = T.mapAccumL enum i tree

    enum ∷ ℤ → TodoItem → (ℤ, TodoItem)
    enum i item = (i + 1, item{itemNumber = i})

itemByNumber ∷ [Todo] → ℤ → Maybe TodoItem
itemByNumber todos i = listToMaybe $ everything (⧺) (listify check) todos
  where
    check ∷ TodoItem → 𝔹
    check item = itemNumber item == i

spawn ∷ String → TodoItem → IO ()
spawn format item = do
  system $ printfItem format item
  return ()

