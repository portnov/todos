{-# LANGUAGE UnicodeSyntax #-}
module Todos.Print where

import Control.Monad
import Control.Monad.Reader
import Data.List
import Data.Tree
import Data.Function (on)
import System.Console.ANSI

import Todos.Unicode
import Todos.Types
import Todos.Config
import Todos.ConfigInstances
import Todos.Formatters

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

showT ∷ SortingType → Int → Todo → [Formatter DefaultConfig]
showT s n (Node item todos) = 
    (sf <++> showId item <++> replicate n ' ' <++> item') :
      (concatMap (showT s (n+2)) $ sortBy' s todos)
  where
    sf ∷ Formatter DefaultConfig
    sf = startFormat

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
  let f = case outOnlyFirst conf of
            False → unlines''
            True  → head
  f $ showT (sorting conf) 0 t

showTodos ∷ [Todo] → Formatter DefaultConfig
showTodos lst = do
  conf ← asks toBaseConfig
  let f = case outOnlyFirst conf of
            False → unlines''
            True  → head
  f $ map showTodo $ sortBy' (sorting conf) $ nub lst

defaultPrintTodos ∷ PrintConfig DefaultConfig → [Todo] → IO ()
defaultPrintTodos cfg lst = 
  let lst' = runReader (showTodos lst) cfg
  in  forM lst' outItem >> putStrLn ""

