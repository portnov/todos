{-# LANGUAGE UnicodeSyntax #-}
module Todos.Print
  (defaultPrintTodos, showTodos)
  where

import Control.Monad
import Control.Monad.Reader
import Data.List
import Data.Tree
import Data.Function (on)
import System.Console.ANSI

import Todos.Unicode
import Todos.Types
import Todos.Config
import Todos.ConfigInstances ()
import Todos.Formatters

sortBy' ∷ SortingType → [Todo] → [Todo]
sortBy' s | s == DoNotSort = id
          | otherwise = sortBy sorter
  where
    sorter = compare `on` (f ∘ rootLabel)
    f = case s of
          DoNotSort → error "Internal error: sortBy' should not be called when DoNotSort is specified!"
          ByTitle → itemName
          ByStatus → itemStatus
          ByTags → unwords ∘ itemTags
          ByStartDate → show ∘ startDate
          ByEndDate → show ∘ endDate
          ByDeadline → show ∘ deadline 

showT ∷ SortingType → Int → String → Todo → [Formatter DefaultConfig]
showT s n sep (Node item todos) = 
    (sf <++> showId item <++> seps <++> item') :
      (concatMap (showT s (n+1) sep) $ sortBy' s todos)
  where
    sf ∷ Formatter DefaultConfig
    sf = startFormat

    seps = concat (replicate n sep)

    item' ∷ Formatter DefaultConfig
    item' = configShow item

    showId :: TodoItem → Formatter DefaultConfig
    showId item = do
      s ← askBase outIds
      c ← askBase outColors
      if s
        then if c 
               then return [OutSetColor Dull Yellow, OutString $ makeId item ++ " ", ResetAll]
               else return [OutString $ makeId item ++ " "]
        else return [OutString ""]

unlines'' ∷ [Formatter c] → Formatter c
unlines'' lst = concat `fmap` (sequence $ intersperse newLine lst)

showTodo ∷ Todo → Formatter DefaultConfig
showTodo t = do
  conf ← asks toBaseConfig
  sep ← askBase indentString
  let f = case outOnlyFirst conf of
            False → unlines''
            True  → head
  f $ showT (sorting conf) 0 sep t

-- | Prepare TODOs for console output
showTodos ∷ [Todo] → Formatter DefaultConfig
showTodos lst = do
  conf ← asks toBaseConfig
  let f = case outOnlyFirst conf of
            False → unlines''
            True  → head
  f $ map showTodo $ sortBy' (sorting conf) $ nub lst

-- | Default function to output TODOs to console
defaultPrintTodos ∷ PrintConfig DefaultConfig → [Todo] → IO ()
defaultPrintTodos cfg lst = 
  let lst' = runReader (showTodos lst) cfg
  in  forM lst' outItem >> putStrLn ""

