{-# LANGUAGE UnicodeSyntax, NoMonomorphismRestriction, TypeSynonymInstances, DeriveDataTypeable #-}
module TodoParser
    (TodoItem (..),
     Todo, TodoMap,
     consTodoMap, pDeps, pItem, parse,
     loadTodo, showTodo
    ) where

import Prelude hiding (putStrLn,readFile)
import System.IO.UTF8

import Data.List
import Text.ParserCombinators.Parsec
import qualified Data.Map as M
import Data.Tree
import Data.Maybe
import Data.Char
import Data.Function
import Data.Generics

import Unicode

data TodoItem = Item {
    itemLevel ∷ Int,
    itemName ∷ String,
    itemTags ∷ [String],
    depends ∷ [String],
    itemStatus ∷ String,
    itemDescr ∷ String}
    deriving (Eq,Data,Typeable)

type Todo = Tree TodoItem

type TodoMap = M.Map String Todo

instance (Ord a) => Ord (Tree a) where
  compare = compare `on` rootLabel

showT n (Node item todos) = (replicate n ' ') ⧺ (show item) ⧺ "\n" ⧺ (concatMap (showT (n+2)) ⋄ sort todos)
showTodo = showT 0

strip = reverse . p . reverse .p
  where
    p = dropWhile isSpace

todoName ∷ Todo → String
todoName todo = itemName ⋄ rootLabel todo

instance Show TodoItem where
    show item = s ⧺ " " ⧺ tags ⧺ name ⧺ ": " ⧺ descr
      where
        n = itemLevel item
        name = itemName item
        ts = itemTags item
        s = itemStatus item
--         deps = unwords ⋄ map show ⋄ depends item
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

pSpace ∷ Parser Char
pSpace = oneOf " \t"

pSpace' ∷ Parser String
pSpace' = do
    pSpace
    return " "

pDeps ∷ Parser [String]
pDeps = do
    string "depends:"
    ws ← (many1 ⋄ noneOf ",\n") `sepBy` (char ',')
    return $ map strip ws

pItem ∷ Parser TodoItem
pItem = do
    s ← many pSpace
    namew ← many1 pWord
    many1 pSpace
    tags ← many1 pWord
    many1 pSpace
    stat ← pWord
    sd ← try $ many pSpace
    let item = Item (length s) (unwords namew) tags [] stat ""
    if null sd
      then return item
      else do
            deps ← option [] (try pDeps)
            many pSpace
            descr ← many (noneOf "\n")
            many pSpace
            many ⋄ char '\n'
            return ⋄ Item (length s) (unwords namew) tags deps stat descr

pWord ∷ Parser String
pWord = do
    w ← many1 (noneOf " \t\n")
    (try pSpace') <|> (return w)
    return w

pItems ∷  GenParser Char () [TodoItem]
pItems = many (try pItem)

loadFile ∷  FilePath → IO [TodoItem]
loadFile path = do
    text ← readFile path
    case parse pItems path text of
        Right items → return items
        Left e → error ⋄ show e

(~-) ∷  TodoItem → Int → TodoItem
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
consTodoMap todos = M.fromList [(todoName todo, todo) | todo ← todos]

stitchTodos ∷ [TodoItem] → [Todo]
stitchTodos items = 
  let m = M.fromList [(todoName todo, todo) | todo ← stitchTodos items]
  in  normalizeList m (mkTodo items)

loadTodo ∷  FilePath → IO TodoMap 
loadTodo path = do
    ts ← loadFile path
    let lst = [(todoName todo, todo) | todo ← stitchTodos ts]
    return ⋄ M.fromList lst
