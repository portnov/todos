{-# LANGUAGE UnicodeSyntax, NoMonomorphismRestriction, FlexibleInstances, TypeSynonymInstances #-}
module Todos.TodoTree 
  (delTag,
   pruneSelector,
   tagPred, statusPred, grepPred, descPred, datePred, idPred,
   forT, mapT,
   defaultPrintTodos)
  where

import Prelude hiding (putStrLn,readFile,getContents,print)
import Todos.IO
import System.Console.ANSI
import Control.Monad
import Control.Monad.Reader
import qualified Data.Map as M
import Data.Generics
import Data.List
import Data.Function (on)
import Data.Tree
import Data.Maybe
import Text.Regex.PCRE
import Data.Hash
import Numeric

import Todos.Types
import Todos.TodoLoader
import Todos.Unicode

sortBy' s | s == DoNotSort = id
          | otherwise = sortBy sorter
  where
    sorter = compare `on` (f ∘ rootLabel)
    f = case s of
          ByTitle → itemName
          ByStatus → itemStatus
          ByTags → unwords ∘ itemTags
          ByStartDate → show ∘ startDate
          ByEndDate → show ∘ endDate
          ByDeadline → show ∘ deadline 

showT ∷ SortingType → Int → Todo → [Formatter]
showT s n (Node item todos) = 
    (startFormat <++> showId item <++> replicate n ' ' <++> configShow item) :
      (concatMap (showT s (n+2)) $ sortBy' s todos)
  where
    showId :: TodoItem → Formatter
    showId item = do
      s ← asks outIds
      c ← asks outColors
      if s
        then if c 
               then return [OutSetColor Yellow, OutString $ makeId item ++ " ", ResetAll]
               else return [OutString $ makeId item ++ " "]
        else return [OutString ""]

unlines'' ∷ [Formatter] → Formatter
unlines'' lst = concat `fmap` (sequence $ intersperse newLine lst)

showTodo ∷ Todo → Formatter
showTodo t = do
  conf ← ask
  let f = case outOnlyFirst conf of
            False → unlines''
            True  → head
  f $ showT (sorting conf) 0 t

showTodos ∷ [Todo] → Formatter
showTodos lst = do
  conf ← ask
  let f = case outOnlyFirst conf of
            False → unlines''
            True  → head
  f $ map showTodo $ sortBy' (sorting conf) $ nub lst

defaultPrintTodos ∷ Config → [Todo] → IO ()
defaultPrintTodos conf lst = 
  let lst' = runReader (showTodos lst) conf
  in  mapM_ outItem lst'

mapTags ∷  (Data a) ⇒ ([String] → [String]) → [a] → [a]
mapTags f = map ⋄ everywhere ⋄ mkT changeTags
  where
    changeTags item@(Item {itemTags=ts}) = item {itemTags = f ts}
        
addTag ∷  (Data a) ⇒ String → [a] → [a]
addTag t = mapTags (t:)

delTag ∷  (Data a) ⇒ String → [a] → [a]
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

addS ∷  (Show a) ⇒ a → TodoItem → TodoItem
addS s item@(Item {itemName=name}) = item {itemName = name ⧺ " — " ⧺ show s}

tagPred ∷  String → TodoItem → 𝔹
tagPred tag = \item → tag ∈ itemTags item

statusPred ∷  String → TodoItem → 𝔹
statusPred st = \item → st == itemStatus item
        
grepPred ∷ String → TodoItem → 𝔹
grepPred pattern = \item → itemName item =~ pattern

descPred ∷ String → TodoItem → 𝔹
descPred pattern = \item → itemDescr item =~ pattern

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
