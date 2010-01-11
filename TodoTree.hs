{-# LANGUAGE UnicodeSyntax, NoMonomorphismRestriction #-}
module TodoTree 
  (addTag, delTag,
   selector, pruneSelector,
   tagPred, statusPred, grepPred,
--    findTag, filterStatus, grep,
   prune,
   forT,
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

import Types
import TodoLoader
import Unicode

showT ∷  (Show t, Ord t) ⇒ Int → Tree t → [String]
showT n (Node item todos) = ((replicate n ' ') ⧺ (show item)):(concatMap (showT (n+2)) $ sort todos)

showTodo ::  (Show t, Ord t) => Bool -> Tree t -> String
showTodo False = unlines ∘ showT 0
showTodo True  = head    ∘ showT 0

showTodos ∷ (Ord t, Show t) ⇒ 𝔹 → [Tree t] → String
showTodos False = unlines ∘ map (showTodo False) ∘ nub
showTodos True  = head    ∘ map (showTodo True) ∘ nub

mapTags f = map ⋄ everywhere ⋄ mkT changeTags
    where
        changeTags item@(Item {itemTags=ts}) = item {itemTags = f ts}
        
addTag t = mapTags (t:)

delTag t = mapTags (delete t)
        
selector ∷ (TodoItem → 𝔹) → (Todo → [Todo])
selector pred (Node item trees) | pred item = [Node item ⋄ concatMap (selector pred) trees]
                                | otherwise = concatMap (selector pred) trees

pruneSelector ∷ ℤ → ℤ → (TodoItem → 𝔹) → (Todo → [Todo])
pruneSelector n m pred = select n 0 False
    where
        select k t b (Node item trees) | t < m       = [Node item ⋄ concatMap (select (n-1) (t+1) True) trees]
                                       | pred item   = [Node item ⋄ concatMap (select (n-1) (t+1) True) trees]
                                       | (k > 0) ∧ b = [Node item ⋄ concatMap (select (k-1) (t+1) True) trees]
                                       | k > 0       = concatMap (select (k-1) (t+1) False) trees
                                       | otherwise   = []                                               

addS s item@(Item {itemName=name}) = item {itemName = name ⧺ " — " ⧺ show s}

tagPred tag = \item → tag ∈ itemTags item

statusPred st = \item → st == itemStatus item
        
grepPred pattern = \item → itemName item =~ pattern

prune ∷ ℤ → ([Todo] → [Todo])
prune n = concatMap ⋄ prune' n
    where
        prune' 0 _ = []
        prune' k (Node item trees) = [Node item ⋄ concatMap (prune' (k-1)) trees]
        
flattern ∷ [Todo] → [Todo]
flattern = concatMap flat
    where
        flat ∷ Todo → [Todo]
        flat (Node item trees) = (Node item []):(concatMap flat trees)

forT ∷ (Monad m) ⇒ [Todo] → (TodoItem → m a) → m ()
forT todos f = forM todos forT' >> return ()
  where
    forT' (Node item trees) =
      do f item
         forM trees forT'
         return ()
