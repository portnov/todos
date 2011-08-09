{-# LANGUAGE UnicodeSyntax, PatternGuards #-}
-- | Read TODOs from files and construct corresponding ADTs.
module Todos.Loader
  (loadTodo)
  where

import Prelude hiding (putStrLn,readFile,getContents,print)
import Prelude.Unicode
import Control.Monad (forM)
import qualified Data.Map as M
import System.FilePath
import Data.Maybe
import Data.Tree
import Data.List (nub, sort)

import Todos.IO
import Todos.Types
import Todos.Config
import Todos.Parser

todoName ∷ Todo → String
todoName todo = itemName $ rootLabel todo

getDepends ∷ TodoMap → TodoItem → [Todo]
getDepends m item = catMaybes [M.lookup name m | name ← depends item] 

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

-- | Almost same that readFile, but also works for special "-" file (stdin)
readFile' ∷ FilePath → IO String
readFile' "-"  = getContents
readFile' file = readFile file

-- | Load items from given file
loadFile ∷ BaseConfig
         → DateTime       -- ^ Current date/time
         → FilePath       -- ^ Path to file
         → IO [TodoItem]
loadFile conf year path =
  case prefix conf of
    Nothing → do
        text ← readFile' path
        return $ parsePlain conf year path text
    Just p  → do
        text ← readFile' path
        return $ parseAlternate conf 2 p year path text

-- | Decrease item level
(~-) ∷  TodoItem → ℤ → TodoItem
i@(Item {itemLevel=n}) ~- k = i {itemLevel=n-k}

-- | Increase item level
(~+) ∷  TodoItem → ℤ → TodoItem
i@(Item {itemLevel=n}) ~+ k = i {itemLevel=n+k}

-- | Check if item level is 0
iszero ∷  TodoItem → 𝔹
iszero item = (itemLevel item)==0

group' ∷  [TodoItem] → [[TodoItem]]
group' [] = []
group' (x:xs) = let (one,other) = break iszero xs
                in (x:one):group' other

mkTodo ∷ [TodoItem] → [Todo]
mkTodo = (map mkTodo') ∘ group'

mkTodo' ∷ [TodoItem] → Todo
mkTodo' [] = error "Internal error: mkTodo' does not sense for empty list!"
mkTodo' (x:xs) = Node x other
    where other = mkTodo $ map (~-lvl) xs
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

-- | Get all (different) tags from Todo list
allTags ∷ [Todo] → [String]
allTags todos = nub $ sort $ concatMap getTags todos
  where
    getTags (Node item children) = itemTags item ⧺ concatMap getTags children

-- | Get all (different) statuses from Todo list
allStatuses ∷ [Todo] → [String]
allStatuses todos = nub $ sort $ concatMap getStatus todos
  where
    getStatus (Node item children) = itemStatus item: concatMap getStatus children

grepBy ∷ (TodoItem → 𝔹) → [Todo] → [Todo]
grepBy cond todos = concatMap grep todos
  where
    grep ∷ Todo → [Todo]
    grep n@(Node _ children) = test n ⧺ concatMap grep children

    test ∷ Todo → [Todo]
    test n@(Node item _)
      | cond item = [n]
      | otherwise = []

grepByTag ∷ String → [Todo] → [Todo]
grepByTag tag todos = grepBy (\item → tag ∈ itemTags item) todos

grepByStatus ∷ String → [Todo] → [Todo]
grepByStatus st todos = grepBy (\item → st == itemStatus item) todos

tagTodo ∷ String → [Todo] → Todo
tagTodo tag todos = Node item $ grepByTag tag todos
  where
    item = Item {
      itemLevel = 0,
      itemPrefix = "",
      itemName = tag,
      itemTags = ["TAG"],
      depends = [],
      itemStatus = ":",
      itemDescr = "",
      startDate = Nothing,
      endDate = Nothing,
      deadline = Nothing,
      fileName = "(no file)",
      lineNr = 0,
      itemNumber = 0}

groupByTag' ∷ [Todo] → [Todo]
groupByTag' todos = 
  map (\t → tagTodo t todos) (allTags todos) ⧺ todos

statusTodo ∷ String → [Todo] → Todo
statusTodo st todos = Node item $ grepByStatus st todos
  where
    item = Item {
      itemLevel = 0,
      itemPrefix = "",
      itemName = st,
      itemTags = ["STATUS"],
      depends = [],
      itemStatus = ":",
      itemDescr = "",
      startDate = Nothing,
      endDate = Nothing,
      deadline = Nothing,
      fileName = "(no file)",
      lineNr = 0,
      itemNumber = 0}

groupByStatus' ∷ [Todo] → [Todo]
groupByStatus' todos =
  map (\s → statusTodo s todos) (allStatuses todos) ⧺ todos

dirname ∷ FilePath → FilePath
dirname path =
  case dropFileName path of
    [] → []
    dir → takeFileName (init dir)

fileTodo ∷ FilePath → TodoItem
fileTodo path = Item {
  itemLevel = 0,
  itemPrefix = "",
  itemName = takeFileName path,
  itemTags = [dirname path],
  depends = [],
  itemStatus = ":",
  itemDescr = path,
  startDate = Nothing,
  endDate = Nothing,
  deadline = Nothing,
  fileName = path,
  lineNr = 0,
  itemNumber = 0}

todosGroup ∷ FilePath → [TodoItem] -> [TodoItem]
todosGroup path items =
  if null items
    then []
    else fileTodo path: map (~+ 1) items

changeTopStatus ∷ Maybe String → [Todo] → [Todo]
changeTopStatus Nothing   todos = todos
changeTopStatus (Just st) todos = map setStatus todos
  where
    setStatus (Node item children) = Node (item {itemStatus = st}) children

-- | Load list of TODO trees from files
loadTodo ∷ BaseConfig
         → DateTime      -- ^ Current date/time
         → [FilePath]    -- ^ List of files
         → IO [Todo]
loadTodo conf date paths = do
    let grp = if groupByFile conf
                then todosGroup
                else const id
    tss ← forM paths $ \path → grp path `fmap` loadFile conf date path
    let todos = stitchTodos (concat tss)
        byTag = if groupByTag conf
                  then groupByTag' todos
                  else todos
        byStatus = if groupByStatus conf
                     then groupByStatus' byTag
                     else byTag
    return $ changeTopStatus (topStatus conf) byStatus

