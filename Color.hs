{-# LANGUAGE UnicodeSyntax #-}
module Color where

import qualified Data.Map as M
import Text.Printf
import Data.Word
import Data.Hash

import Unicode
import Types

data HSV = HSV {
  colorHue ∷ Double,
  colorSaturation ∷ Double,
  colorValue ∷ Double }
  deriving (Eq)

instance Show HSV where
  show (HSV h s v) = printf "\"%.3f %.3f %.3f\"" h s v

(<#>) ∷ HSV → HSV → HSV
(HSV h1 s1 v1) <#> (HSV h2 s2 v2) = HSV ((h1+h2)/2) ((s1+s2)/2) ((v1+v2)/2)

tagHues ∷ M.Map String Double
tagHues = M.fromList $ [
  ("BUG",   0.07),
  ("NOTE",  0.25),
  ("ERROR", 0.0),
  ("TAG",   0.16),
  ("TODO",  0.55)]

hashAsDouble ∷ Hashable a ⇒ a → Double
hashAsDouble x = (fromIntegral $ asWord64 $ hash x) / (fromIntegral (maxBound :: Word64))

tagHue ∷ String → Double
tagHue tag =
  case M.lookup tag tagHues of
    Nothing → hashAsDouble tag
    Just h  → h

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

statusSat ∷ String → Double
statusSat st = 
  case M.lookup st statusSats of
    Nothing → 0.4* hashAsDouble st
    Just s  → s

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

statusHue ∷ String → Double
statusHue st =
  case M.lookup st statusHues of
    Nothing → hashAsDouble st
    Just h  → h

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

