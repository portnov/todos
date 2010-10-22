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
import System.FilePath
import Data.Maybe
import Data.Tree
import Data.List (init, nub, sort)

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

(~+) ∷  TodoItem → ℤ → TodoItem
i@(Item {itemLevel=n}) ~+ k = i {itemLevel=n+k}

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

allTags ∷ [Todo] → [String]
allTags todos = nub $ sort $ concatMap getTags todos
  where
    getTags (Node item children) = itemTags item ⧺ concatMap getTags children

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
      itemName = tag,
      itemTags = ["TAG"],
      depends = [],
      itemStatus = ":",
      itemDescr = "",
      startDate = Nothing,
      endDate = Nothing,
      deadline = Nothing,
      fileName = "(no file)",
      lineNr = 0 }

groupByTag' ∷ [Todo] → [Todo]
groupByTag' todos = 
  map (\t → tagTodo t todos) (allTags todos) ⧺ todos

statusTodo ∷ String → [Todo] → Todo
statusTodo st todos = Node item $ grepByStatus st todos
  where
    item = Item {
      itemLevel = 0,
      itemName = st,
      itemTags = ["STATUS"],
      depends = [],
      itemStatus = ":",
      itemDescr = "",
      startDate = Nothing,
      endDate = Nothing,
      deadline = Nothing,
      fileName = "(no file)",
      lineNr = 0 }

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
  itemName = takeFileName path,
  itemTags = [dirname path],
  depends = [],
  itemStatus = ":",
  itemDescr = path,
  startDate = Nothing,
  endDate = Nothing,
  deadline = Nothing,
  fileName = path,
  lineNr = 0 }

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
loadTodo ∷ Config
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

