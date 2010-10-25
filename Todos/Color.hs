{-# LANGUAGE UnicodeSyntax #-}
module Todos.Color where

import qualified Data.Map as M
import Text.Printf
import Data.Word
import Data.Hash
import Data.List (minimumBy)
import Data.Function (on)
import qualified System.Console.ANSI as ANSI

import Todos.Unicode
import Todos.Types

-- | Hue, saturation and value
data HSV = HSV {
  colorHue ∷ Double,
  colorSaturation ∷ Double,
  colorValue ∷ Double }
  deriving (Eq)

instance Show HSV where
  show (HSV h s v) = printf "\"%.3f %.3f %.3f\"" h s v

-- | Correspondence between console colors and HSV colors
consoleColors ∷ [((ANSI.ColorIntensity, ANSI.Color), HSV)]
consoleColors = 
  [((ANSI.Dull, ANSI.Black),  HSV 0 0 0),
   ((ANSI.Dull, ANSI.Red),    HSV 0 1 0.67),
   ((ANSI.Dull, ANSI.Green),  HSV 0.333 1 0.67),
   ((ANSI.Dull, ANSI.Yellow), HSV (1.0/12.0) 1 0.67),
   ((ANSI.Dull, ANSI.Blue),   HSV 0.667 1 0.67),
   ((ANSI.Dull, ANSI.Magenta), HSV (5.0/6.0) 1 0.67),
   ((ANSI.Dull, ANSI.Cyan),   HSV 0.5 1 0.67),
   ((ANSI.Dull, ANSI.White),  HSV 0 0 0.67),
   ((ANSI.Vivid, ANSI.Black),  HSV 0 0 0.33),
   ((ANSI.Vivid, ANSI.Red),    HSV 0 0.67 1),
   ((ANSI.Vivid, ANSI.Green),  HSV 0.333 0.67 1),
   ((ANSI.Vivid, ANSI.Yellow), HSV (1.0/12.0) 0.67 1),
   ((ANSI.Vivid, ANSI.Blue),   HSV 0.667 0.67 1),
   ((ANSI.Vivid, ANSI.Magenta), HSV (5.0/6.0) 0.67 1),
   ((ANSI.Vivid, ANSI.Cyan),   HSV 0.5 0.67 1),
   ((ANSI.Vivid, ANSI.White),  HSV 0 0 1) ]

-- | Get console color which is nearest to given HSV color
consoleColor ∷ HSV → (ANSI.ColorIntensity, ANSI.Color)
consoleColor hsv = fst $ minimumBy (compare `on` ρ) consoleColors
  where ρ (_, clr) = sum $ map (^2) [colorHue hsv - colorHue clr,
                                     colorSaturation hsv - colorSaturation clr,
                                     colorValue hsv - colorValue clr ]

-- | Hue values for some common tags
tagHues ∷ M.Map String Double
tagHues = M.fromList $ [
  ("BUG",   0.07),
  ("NOTE",  0.25),
  ("ERROR", 0.0),
  ("TAG",   0.16),
  ("TODO",  0.55)]

hashAsDouble ∷ Hashable a ⇒ a → Double
hashAsDouble x = (fromIntegral $ asWord64 $ hash x) / (fromIntegral (maxBound :: Word64))

-- | Get color hue from tag name
tagHue ∷ String → Double
tagHue tag =
  case M.lookup tag tagHues of
    Nothing → hashAsDouble tag
    Just h  → h

-- | Color saturation values for some common statuses
statusSats ∷ M.Map String Double
statusSats = M.fromList $ [
  ("*",      1.0),
  ("URGENT", 1.0),
  ("+",      0.9),
  ("W",      0.9),
  ("?",      0.8),
  ("Q",      0.8),
  ("M",      0.7),
  ("L",      0.6),
  ("DONE",   0.5),
  ("FIXED",  0.5),
  ("WORKSFORME", 0.45),
  (":",      0.4),
  ("-",      0.3),
  ("INVALID", 0.3),
  ("o",      0.3),
  ("x",      0.2)]

-- | Get color saturation from item status
statusSat ∷ String → Double
statusSat st = 
  case M.lookup st statusSats of
    Nothing → 0.4* hashAsDouble st
    Just s  → s

-- | Color hue values for some common item statuses
statusHues ∷ M.Map String Double
statusHues = M.fromList $ [
  ("*",      0.0),
  ("URGENT", 0.0),
  ("+",      0.16),
  ("W",      0.08),
  ("?",      0.7),
  ("Q",      0.8),
  ("M",      0.2),
  ("L",      0.6),
  ("DONE",   0.25),
  ("FIXED",  0.25),
  ("NOTE",   0.3),
  ("WORKSFORME", 0.1),
  ("INVALID", 0.45),
  (":",      0.4),
  ("-",      0.09),
  ("o",      0.8),
  ("x",      0.2)]

-- | Get color hue from item status
statusHue ∷ String → Double
statusHue st =
  case M.lookup st statusHues of
    Nothing → hashAsDouble st
    Just h  → h

-- | Get console color for item status
statusColor ∷ String → (ANSI.ColorIntensity, ANSI.Color)
statusColor st = consoleColor $ HSV (statusHue st) (statusSat st) 0.2

-- | Get console color for item name (this is const Nothing)
defItemConsoleColor ∷ TodoItem → Maybe (ANSI.ColorIntensity, ANSI.Color)
defItemConsoleColor _ = Nothing

-- | Get color for item (this is used in DOT output)
getColor ∷ TodoItem → HSV
getColor item = HSV h s v 
  where
    tags = itemTags item
    n    = length tags
    st   = itemStatus item
    h    = case n of
             0 → statusHue st
             _ → (statusHue st + sum (map tagHue tags))/fromIntegral (n+1)
    s    = statusSat st
    v    = 0.8

