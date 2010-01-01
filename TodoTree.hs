{-# LANGUAGE UnicodeSyntax #-}
module TodoTree 
  (addTag, delTag,
   filterMap, selector, pruneSelector,
   findTag, filterStatus, grep, prune,
   flattern,
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

mapTags f = M.map ⋄ everywhere ⋄ mkT changeTags
    where
        changeTags item@(Item {itemTags=ts}) = item {itemTags = f ts}
        
addTag t = mapTags (t:)

delTag t = mapTags (delete t)
        
filterMap ∷ (Todo → [Todo]) → TodoMap → TodoMap
filterMap selector m = consTodoMap ⋄ concatMap selector ⋄ M.elems m

selector ∷ (TodoItem → 𝔹) → (Todo → [Todo])
selector pred (Node item trees) | pred item  = [Node item ⋄ concatMap (selector pred) trees]
                                               | otherwise = concatMap (selector pred) trees

pruneSelector n pred = select n False
    where
        select k b (Node item trees) | pred item   = [Node item ⋄ concatMap (select (n-1) True) trees]
                                     | (k > 0) ∧ b = [Node item ⋄ concatMap (select (k-1) True) trees]
                                     | k > 0       = concatMap (select (k-1) False) trees
                                     | otherwise   = []                                               

addS s item@(Item {itemName=name}) = item {itemName = name ⧺ " — " ⧺ show s}

findTag n tag = filterMap ⋄ tagFinder tag
    where
        tagFinder tag = pruneSelector n ⋄ \item → tag ∈ itemTags item

filterStatus n st = filterMap ⋄ statusSelector st
    where
        statusSelector st = pruneSelector n ⋄ \item → st == itemStatus item
        
grep n pattern = filterMap grepper 
    where
        grepper = pruneSelector n ⋄ \item → itemName item =~ pattern

prune n = filterMap ⋄ prune' n
    where
        prune' 0 _ = []
        prune' k (Node item trees) = [Node item ⋄ concatMap (prune' (k-1)) trees]
        
flattern ∷ TodoMap → TodoMap
flattern = filterMap flat
    where
        flat ∷ Todo → [Todo]
        flat (Node item trees) = (Node item []):(concatMap flat trees)
        
showTodos = concatMap showTodo ∘ M.elems

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

