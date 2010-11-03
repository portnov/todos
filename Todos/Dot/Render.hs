{-# LANGUAGE UnicodeSyntax, TypeSynonymInstances, FlexibleInstances #-}
-- | Output TODOs tree as DOT graph
module Todos.Dot.Render
  (showAsDot)
  where

import Data.List
import Data.Tree
import Text.Printf

import Todos.Unicode
import Todos.Types
import Todos.Dot.Color
import Todos.Dot.Shapes

data Dot = Dot {
  dotVertices ∷ [TodoItem],
  dotEdges ∷ [(TodoItem, TodoItem)],
  dotSubgraphs ∷ [Subgraph]
  }

data Subgraph = Subgraph {
  subLabel ∷ String,
  subItems ∷ [TodoItem] }
  deriving (Eq)

toDot ∷ Todo → Dot
toDot todo = Dot (getVertices todo) (getEdges todo) (getSubgraphs todo)

getVertices ∷ Todo → [TodoItem]
getVertices (Node item forest) 
  | itemStatus item == "GROUP" = concatMap getVertices forest
  | otherwise = [item] ⧺ concatMap getVertices forest

getEdges ∷ Todo → [(TodoItem, TodoItem)]
getEdges (Node item forest) 
  | itemStatus item == "GROUP" = concatMap getEdges forest
  | otherwise = [(item, rootLabel child) | child ← forest] ⧺ concatMap getEdges forest

getSubgraphs ∷ Todo → [Subgraph]
getSubgraphs (Node item forest)
  | itemStatus item == "GROUP" = Subgraph (showItem item) (nub $ sort $ concatMap flattern forest) :
                                  concatMap getSubgraphs forest
  | otherwise = []
  where 
    showItem item = showTags item ⧺ itemName item ⧺ "\\n" ⧺ itemDescr item
    showTags item | null (itemTags item) = ""
                  | otherwise = "[" ⧺ unwords (itemTags item) ⧺ "] "
    flattern (Node item children) = item: concatMap flattern children

instance Show Dot where
  show (Dot vs es subs) = "digraph Todo {\n"
                   ⧺ unlines (map (showDotNode getColor getShape) vs)
                   ⧺ unlines (map showDotEdge es)
                   ⧺ unlines (map showSubgraph subs)
                   ⧺ "}\n"

showD ∷ (TodoItem → HSV) → (TodoItem → Shape) → [Dot] → String
showD colorFn shapeFn dots
            = "digraph Todo {\n"
            ⧺ "  rankdir = \"RL\";\n"
            ⧺ "  node [shape=\"box\", style=\"filled\"];\n"
            ⧺ unlines (map (showDotNode colorFn shapeFn) $ nub $ sort $ concatMap dotVertices dots)
            ⧺ unlines (map showDotEdge $ nub $ sort $ concatMap dotEdges dots)
            ⧺ unlines (map showSubgraph $ nub $ concatMap dotSubgraphs dots)
            ⧺ "}\n"

makeName ∷ TodoItem → String
makeName item = "\"" ⧺ makeId item ⧺ "\""

showDotNode ∷ (TodoItem → HSV) → (TodoItem → Shape) → TodoItem → String
showDotNode colorFn shapeFn item =
  printf "  %s [label=\"%s\\n%s\\n%s\", fillcolor=%s, shape=\"%s\"];" (makeName item) (itemStatus item) (unwords $ itemTags item) (itemName item) (show $ colorFn item) (show $ shapeFn item)

showDotEdge ∷ (TodoItem, TodoItem) → String
showDotEdge (x,y) 
  | itemStatus x == "GROUP" = ""
  | otherwise               = printf "  %s -> %s;" (makeName y) (makeName x)

showSubgraph ∷ Subgraph → String
showSubgraph (Subgraph label items) 
  | null items = ""
  | otherwise = "  subgraph \"cluster_" ⧺ makeId label ⧺ "\" {\n" 
              ⧺ "    label=\"" ⧺ label ⧺ "\";\n"
              ⧺ (unlines $ map ("    " ⧺) $ map showItem items)
              ⧺ "\n  }"
  where
    showItem item = makeName item ⧺ ";"

-- | Return DOT output for Todos
showAsDot ∷ (TodoItem → HSV)   -- ^ Function to determine node color
          → (TodoItem → Shape) -- ^ Function to determine node shape
          → [Todo]             -- ^ Todo list
          → String
showAsDot colorFn shapeFn todos = (showD colorFn shapeFn) (map toDot todos)

