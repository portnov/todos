{-# LANGUAGE UnicodeSyntax, NoMonomorphismRestriction, TypeSynonymInstances, DeriveDataTypeable #-}
module TodoParser
--     (TodoItem (..),
--      Todo, TodoMap, flattern,
--      consTodoMap,
--      loadTodo, showTodo
--     )
    where

import Prelude hiding (putStrLn,readFile,getContents)
import System.IO.UTF8
import Control.Monad
import Data.List
import Text.ParserCombinators.Parsec
import qualified Data.Map as M
import Data.Tree
import Data.Maybe
import Data.Char
import Data.Function
import Data.Generics

import Unicode
import Types

showT ::  (Show t, Ord t) => Int -> Tree t -> [String]
showT n (Node item todos) = ((replicate n ' ') ⧺ (show item)):(concatMap (showT (n+2)) $ sort todos)

showTodo ::  (Show t, Ord t) => Bool -> Tree t -> String
showTodo False = unlines ∘ showT 0
showTodo True  = head    ∘ showT 0

strip = reverse . p . reverse .p
  where
    p = dropWhile isSpace

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

pSpace ∷ Parser Char
pSpace = oneOf " \t"

pSpace' ∷ Parser String
pSpace' = do
    pSpace
    return " "

pDeps ∷ Parser [String]
pDeps = do
    string "("
    ws ← (many1 ⋄ noneOf ",)\n") `sepBy` (char ',')
    string ")"
    return $ map strip ws

pTags ∷ Parser [String]
pTags = do
    ts ← between (char '[') (char ']') $ word `sepBy1` pSpace
    many pSpace
    return ts
  where
    word = many1 (noneOf " \t\n]")

pItem ∷ Parser TodoItem
pItem = do
    pos ← getPosition
    s ← many pSpace
    stat ← pWord
    tags ← (try pTags <|> return [])
    namew ← many1 pWord
    many pSpace
    deps ← (try pDeps <|> return [])
    many pSpace
    descr ← many (noneOf "\n")
    many pSpace
    many ⋄ char '\n'
    return ⋄ Item (fromIntegral $ length s) (unwords namew) tags deps stat descr (sourceName pos) (sourceLine pos)

pWord ∷ Parser String
pWord = do
    w ← many1 (noneOf " \t\n")
    (try pSpace') <|> (return w)
    return w

pItems ∷  GenParser Char () [TodoItem]
pItems = do
  its ← many (try pItem)
  eof
  return its

readFile' ∷ FilePath → IO String
readFile' "-" = getContents
readFile' file = readFile file

loadFile ∷  FilePath → IO [TodoItem]
loadFile path = do
    text ← readFile' path
    case parse pItems path text of
        Right items → return items
        Left e → error ⋄ show e

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
          
flattern ∷ [Todo] → [Todo]
flattern = concatMap flat
    where
        flat ∷ Todo → [Todo]
        flat (Node item trees) = (Node item []):(concatMap flat trees)
        
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

loadTodo ∷  [FilePath] → IO [Todo]
loadTodo paths = do
    tss ← forM paths loadFile
    return $ stitchTodos (concat tss)
