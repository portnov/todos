{-# LANGUAGE UnicodeSyntax #-}
-- | Operations with dates
module Todos.Dates
  (parseDate, getCurrentDateTime,
   pSpecDates)
  where

import Data.Dates
import Text.Parsec

import Todos.Types

dateType ∷ String → DateType
dateType "start" = StartDate
dateType "end"   = EndDate
dateType "deadline" = Deadline
dateType _ = error "unknown date type"

-- | Parse date/time with date type
pSpecDate ∷ DateTime → Parsec String st (DateType, DateTime)
pSpecDate date = do
  tp ← choice $ map string ["start","end","deadline"]
  string ": "
  dt ← pDateTime date
  return (dateType tp, dt)

-- | Parse set of dates with types (in parenthesis)
pSpecDates ∷ DateTime → Parsec String st [(DateType, DateTime)]
pSpecDates date = do
  char '('
  pairs ← (pSpecDate date) `sepBy1` (string "; ")
  string ") "
  return pairs

