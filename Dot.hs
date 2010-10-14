{-# LANGUAGE UnicodeSyntax, TypeSynonymInstances, FlexibleInstances #-}
module Dot
  (showAsDot)
  where

import Data.List
import Data.Tree
import Text.Printf

import Unicode
import Types
import Color
import Shapes

data Dot = Dot {
  dotVertices ∷ [TodoItem],
  dotEdges ∷ [(TodoItem, TodoItem)]
  }

toDot ∷ Todo → Dot
toDot todo = Dot (getVertices todo) (getEdges todo)

getVertices ∷ Todo → [TodoItem]
getVertices (Node item forest) =
  [item] ⧺ concatMap getVertices forest

getEdges ∷ Todo → [(TodoItem, TodoItem)]
getEdges (Node item forest) =
  [(item, rootLabel child) | child ← forest] ⧺ concatMap getEdges forest

instance Show Dot where
  show (Dot vs es) = "digraph Todo {\n"
                   ⧺ unlines (map showDotNode vs)
                   ⧺ unlines (map showDotEdge es)
                   ⧺ "}\n"

showD ∷ [Dot] → String
showD dots = "digraph Todo {\n"
            ⧺ "  rankdir = \"RL\";\n"
            ⧺ "  node [shape=\"box\", style=\"filled\"];\n"
            ⧺ unlines (map showDotNode $ nub $ sort $ concatMap dotVertices dots)
            ⧺ unlines (map showDotEdge $ nub $ sort $ concatMap dotEdges dots)
            ⧺ "}\n"

makeName ∷ TodoItem → String
makeName item = "\"" ⧺ makeId item ⧺ "\""

showDotNode ∷ TodoItem → String
showDotNode item =
  printf "  %s [label=\"%s\\n%s\\n%s\", fillcolor=%s, shape=\"%s\"];" (makeName item) (itemStatus item) (unwords $ itemTags item) (itemName item) (show $ getColor item) (show $ getShape item)

showDotEdge ∷ (TodoItem, TodoItem) → String
showDotEdge (x,y) = printf "  %s -> %s;" (makeName y) (makeName x)

showAsDot ∷ [Todo] → String
showAsDot todos = showD (map toDot todos)

