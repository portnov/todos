{-# LANGUAGE UnicodeSyntax, PatternGuards #-}
-- | Read TODOs from files and construct corresponding ADTs.
module TodoLoader
  (loadTodo)
  where

import Prelude hiding (putStrLn,readFile,getContents,print)
import IO
import Control.Monad (forM)
import qualified Data.Map as M
import Text.ParserCombinators.Parsec
import Data.Maybe
import Data.Tree

import Unicode
import Types
import TodoParser

todoName ∷ Todo → String
todoName todo = itemName ⋄ rootLabel todo

getDepends ∷ TodoMap → TodoItem → [Todo]
getDepends m item = catMaybes [M.lookup name m | name ← depends item] 

normalizeItem ∷ TodoMap → TodoItem → Todo
normalizeItem m item = Node item (map (normalize m) ⋄ getDepends m item)

normalize ∷ TodoMap → Todo → Todo 
normalize m todo = Node item' ((map (normalize m) subTodos) ⧺ (map (normalize m) deps))
  where
    item ∷ TodoItem
    item = rootLabel todo

    item' ∷ TodoItem
    item' = item {depends=[]}

    subTodos ∷ [Todo]
    subTodos = subForest todo

    deps ∷ [Todo]
    deps = getDepends m item

normalizeList ∷ TodoMap → [Todo] → [Todo]
normalizeList m todos = map (normalize m) todos

readFile' ∷ FilePath → IO String
readFile' "-"  = getContents
readFile' file = readFile file

loadFile ∷ Config
         → DateTime
         → FilePath
         → IO [TodoItem]
loadFile conf year path =
  case prefix conf of
    Nothing → do
        text ← readFile' path
        return $ parsePlain conf year path text
    Just p  → do
        text ← readFile' path
        return $ parseAlternate conf 2 p year path text

(~-) ∷  TodoItem → ℤ → TodoItem
i@(Item {itemLevel=n}) ~- k = i {itemLevel=n-k}

iszero ∷  TodoItem → 𝔹
iszero item = (itemLevel item)==0

group' ∷  [TodoItem] → [[TodoItem]]
group' [] = []
group' (x:xs) = let (one,other) = break iszero xs
                in (x:one):group' other

mkTodo ∷ [TodoItem] → [Todo]
mkTodo = (map mkTodo') ∘ group'

mkTodo' ∷ [TodoItem] → Todo
mkTodo' (x:xs) = Node x other
    where other = mkTodo ⋄ map (~-lvl) xs
          lvl = itemLevel (head xs)
          
consTodoMap ∷ [Todo] → TodoMap
consTodoMap todos = M.fromList (cons1 100 todos)
  where
    cons1 ∷ Int → [Todo] → [(String,Todo)]
    cons1 0 _ = []
    cons1 max trees = [(todoName todo, todo) | todo ← trees] ⧺ cons1 (max-1) (children trees)
    children ∷ [Todo] → [Todo]
    children trees = concatMap subForest trees

stitchTodos ∷ [TodoItem] → [Todo]
stitchTodos items = 
  let m = consTodoMap t
      t = mkTodo items
  in  normalizeList m t

-- | Load list of TODO trees from files
loadTodo ∷ Config
         → DateTime      -- ^ Current date/time
         → [FilePath]    -- ^ List of files
         → IO [Todo]
loadTodo conf date paths = do
    tss ← forM paths (loadFile conf date)
    return $ stitchTodos (concat tss)
