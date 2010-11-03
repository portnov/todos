{-# LANGUAGE UnicodeSyntax #-}
-- | Support for DOT node shapes
module Todos.Dot.Shapes where

import qualified Data.Map as M

import Todos.Types

-- | Supported node shapes for DOT output
data Shape = 
    Box 
  | Box3D
  | Component
  | Square
  | MSquare
  | Ellipse
  | Diamond
  | MDiamond
  | Circle
  | DCircle
  | MCircle
  | Note
  | Parallelogram
  | Tab
  | Folder
  | Polygon Int
  | Point
  | Egg
  | Triangle Bool    -- ^ Inverted?
  | PlainText
  | Trapezium Bool   -- ^ Inverted?
  | House Bool       -- ^ Inverted?
  | Pentagon
  | Hexagon
  | Septagon
  | Octagon Int      -- ^ Simple, double, triple?
  deriving (Eq)

instance Show Shape where
  show DCircle       = "doublecircle"
  show (Polygon k)   = "polygon\", sides=\"" ++ show k 
  show (Triangle True) = "invtriangle"
  show (Triangle False) = "triangle"
  show (Trapezium True) = "invtrapezium"
  show (Trapezium False) = "trapezium"
  show (House True)  = "invhouse"
  show (House False) = "house"
  show (Octagon 1)   = "octagon"
  show (Octagon 2)   = "doubleoctagon"
  show (Octagon 3)   = "tripleoctagon"
  show (Octagon _)   = error "Only 1,2 or 3 are supported as Octagon arguments!"
  show Box  = "box"
  show Box3D = "box3d"
  show Component = "component"
  show Square = "square"
  show MSquare = "Msquare"
  show Ellipse = "ellipse"
  show Diamond = "diamond"
  show Circle = "circle"
  show MDiamond = "Mdiamond"
  show MCircle = "Mcircle"
  show Note = "note"
  show Parallelogram = "parallelogram"
  show Tab = "tab"
  show Folder = "folder"
  show Point = "point"
  show Egg = "egg"
  show PlainText = "plaintext"
  show Pentagon = "pentagon"
  show Hexagon = "hexagon"
  show Septagon = "septagon"

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

