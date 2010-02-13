{-# LANGUAGE UnicodeSyntax, NoMonomorphismRestriction, FlexibleInstances, TypeSynonymInstances #-}
module TodoTree 
--   (delTag,
--    pruneSelector,
--    tagPred, statusPred, grepPred,
--    forT, mapT,
--    printTodos)
  where

-- import Prelude hiding (putStrLn,putStr)
-- import System.IO.UTF8
import Control.Monad
import Control.Monad.Reader
import qualified Data.Map as M
import Data.Generics
import Data.List
import Data.Tree
import Data.Maybe
import Text.Regex.PCRE

import Types
import TodoLoader
import Unicode

showT ∷ (ConfigShow t, Ord t) ⇒ Int → Tree t → [ConfigM]
showT n (Node item todos) = 
  (configM <++> (replicate n ' ') <++> (configShow item)) :
    (concatMap (showT (n+2)) $ sort todos)

unlines'' ∷ [ConfigM] → ConfigM
unlines'' lst = concat `fmap` (sequence $ intersperse newLine lst)

showTodo ∷ (ConfigShow t, Ord t) ⇒ Tree t → ConfigM
showTodo t = do
  conf ← ask
  let f = case outOnlyFirst conf of
            False → unlines''
            True  → head
  f $ showT 0 t

showTodos ∷ (ConfigShow t, Ord t) ⇒ [Tree t] → Reader Config [OutItem]
showTodos lst = do
  conf ← ask
  let f = case outOnlyFirst conf of
            False → unlines''
            True  → head
  f (map showTodo $ nub lst)

printTodos ∷ (ConfigShow t, Ord t) ⇒ Config → [Tree t] → IO ()
printTodos conf lst = 
  let lst' = runReader (showTodos lst) conf
  in  mapM_ outItem lst'

mapTags f = map ⋄ everywhere ⋄ mkT changeTags
  where
    changeTags item@(Item {itemTags=ts}) = item {itemTags = f ts}
        
addTag t = mapTags (t:)

delTag t = mapTags (delete t)

pruneSelector ∷ (TodoItem → 𝔹) → Transformer
pruneSelector pred = do
  (Limit n) ← asks pruneL
  (Limit m) ← asks minL
  return $ pruneSelector' n m pred
        
pruneSelector' ∷ ℤ → ℤ → (TodoItem → 𝔹) → (Todo → [Todo])
pruneSelector' n m pred = select n 0 False
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

flattern ∷ [Todo] → [Todo]
flattern = concatMap flat
    where
        flat ∷ Todo → [Todo]
        flat (Node item trees) = (Node item []):(concatMap flat trees)

forT ∷ (Monad m, Eq t) ⇒ [Tree t] → (t → m a) → m [b]
forT todos f = forM (nub todos) forT'
  where
    forT' (Node item trees) =
      do f item
         res ← forM trees forT'
         return $ last res

mapT ∷ (t → t) → [Tree t] → [Tree t]
mapT f todos = map mapT' todos
  where
    mapT' (Node item trees) = Node (f item) (mapT f trees)
