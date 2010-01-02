{-# LANGUAGE UnicodeSyntax #-}
module TodoTree 
  (addTag, delTag,
   selector, pruneSelector,
   tagPred, statusPred, grepPred,
   findTag, filterStatus, grep, prune,
   showTodos)
  where

import Prelude hiding (putStrLn)
import System.IO.UTF8
import System (getArgs)
import System.Console.GetOpt
import Control.Monad
import qualified Data.Map as M
import Data.Generics
import Data.List
import Data.Tree
import Data.Maybe
import Text.Regex.PCRE

import TodoParser
import Unicode

mapTags f = map ⋄ everywhere ⋄ mkT changeTags
    where
        changeTags item@(Item {itemTags=ts}) = item {itemTags = f ts}
        
addTag t = mapTags (t:)

delTag t = mapTags (delete t)
        
-- filterMap ∷ (Todo → [Todo]) → TodoMap → TodoMap
-- filterMap selector m = consTodoMap ⋄ concatMap selector ⋄ M.elems m

selector ∷ (TodoItem → 𝔹) → (Todo → [Todo])
selector pred (Node item trees) | pred item  = [Node item ⋄ concatMap (selector pred) trees]
                                               | otherwise = concatMap (selector pred) trees

pruneSelector ∷ ℤ → (TodoItem → 𝔹) → (Todo → [Todo])
pruneSelector n pred = select n False
    where
        select k b (Node item trees) | pred item   = [Node item ⋄ concatMap (select (n-1) True) trees]
                                     | (k > 0) ∧ b = [Node item ⋄ concatMap (select (k-1) True) trees]
                                     | k > 0       = concatMap (select (k-1) False) trees
                                     | otherwise   = []                                               

addS s item@(Item {itemName=name}) = item {itemName = name ⧺ " — " ⧺ show s}

findTag ∷ ℤ → String → ([Todo] → [Todo])
findTag n tag = concatMap ⋄ tagFinder tag
    where
        tagFinder tag = pruneSelector n ⋄ tagPred tag

tagPred tag = \item → tag ∈ itemTags item

filterStatus ∷ ℤ → String → ([Todo] → [Todo])
filterStatus n st = concatMap ⋄ statusSelector st
    where
        statusSelector st = pruneSelector n ⋄ statusPred st 

statusPred st = \item → st == itemStatus item
        
grep ∷ ℤ → String → ([Todo] → [Todo])
grep n pattern = concatMap grepper 
    where
        grepper = pruneSelector n ⋄ grepPred pattern

grepPred pattern = \item → itemName item =~ pattern

prune ∷ ℤ → ([Todo] → [Todo])
prune n = concatMap ⋄ prune' n
    where
        prune' 0 _ = []
        prune' k (Node item trees) = [Node item ⋄ concatMap (prune' (k-1)) trees]
        
showTodos = concatMap showTodo ∘ nub

main = do
  todos ← loadTodo "test.txt"
  let todos' = delTag "-" todos
  mapM (\t → do 
                         putStrLn ⋄ showTodos t
                         putStrLn "-----------------------------") ⋄ [
            todos',
            findTag 2 "TODO" todos',
            prune 2 todos',
            filterStatus 1 "X" todos',
            grep 1 "Second" todos',
            flattern todos',
            grep 2 "Todo" todos']

