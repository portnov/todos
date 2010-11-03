{-# LANGUAGE UnicodeSyntax #-}
module Todos.Shapes where

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
  show Box  = "Box "
  show Box3D = "Box3D"
  show Component = "Component"
  show Square = "Square"
  show MSquare = "MSquare"
  show Ellipse = "Ellipse"
  show Diamond = "Diamond"
  show Circle = "Circle"
  show MDiamond = "MDiamond"
  show MCircle = "MCircle"
  show Note = "Note"
  show Parallelogram = "Parallelogram"
  show Tab = "Tab"
  show Folder = "Folder"
  show Point = "Point"
  show Egg = "Egg"
  show PlainText = "PlainText"
  show Pentagon = "Pentagon"
  show Hexagon = "Hexagon"
  show Septagon = "Septagon"

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

