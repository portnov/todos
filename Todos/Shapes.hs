{-# LANGUAGE UnicodeSyntax #-}
module Todos.Shapes where

import qualified Data.Map as M

import Todos.Types

-- | Supported node shapes for DOT output
data Shape = 
    Box 
  | Ellipse
  | Diamond
  | DCircle
  | Note
  | Parallelogram
  | Folder
  deriving (Eq)

instance Show Shape where
  show Box           = "box"
  show Ellipse       = "ellipse"
  show Diamond       = "diamond"
  show DCircle       = "doublecircle"
  show Note          = "note"
  show Parallelogram = "parallelogram"
  show Folder        = "folder"

-- | Node shapes for some common item statuses
shapes ∷ M.Map String Shape
shapes = M.fromList $ [
  ("o", Ellipse),
  ("O", DCircle),
  (":", Folder),
  ("*", Diamond),
  ("/", Parallelogram),
  ("NOTE", Note) ]

-- | Get item shape for this item (default funciton)
getShape ∷ TodoItem → Shape
getShape item = 
  case M.lookup (itemStatus item) shapes of
    Nothing → Box
    Just s  → s

