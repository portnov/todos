{-# LANGUAGE UnicodeSyntax #-}
module Dates
  (pDate, parseDate, pSpecDates)
  where

import Data.Char (toUpper)
import Data.Function (on)
import Text.ParserCombinators.Parsec

import Types

eqI ∷  Char → Char → Bool
eqI = (==) `on` toUpper

startsWith ∷  String → String → Bool
startsWith [] _ = False
startsWith _ [] = True
startsWith (x:xs) (y:ys) = (x `eqI` y) && (xs `startsWith` ys)

lookupS ∷ String → [(String,a)] → Maybe a
lookupS _ [] = Nothing
lookupS k ((k',v):other) | k' `startsWith` k = Just v
                         | otherwise         = lookupS k other

monthsN ∷ [(String,Int)]
monthsN = zip months [1..]

lookupMonth ∷ String → Maybe Int
lookupMonth n = lookupS n monthsN

date y m d = DateTime y m d 0 0 0

addTime dt t = dt {
                 hour = tHour t + hour dt,
                 minute = tMinute t + minute dt,
                 second = tSecond t + second dt }

times ∷ Int → Parser t → Parser [t]
times 0 _ = return []
times n p = do
  ts ← times (n-1) p
  t ← optionMaybe p
  case t of
    Just t' → return (ts ++ [t'])
    Nothing → return ts
                               
number ∷ Int → Int → Parser Int
number n m = do
  t ← read `fmap` (n `times` digit)
  if t > m
    then fail "number too large"
    else return t

pYear ∷ Parser Int
pYear = do
  y ← number 4 10000
  if y < 2000
    then return (y+2000)
    else return y

pMonth ∷ Parser Int
pMonth = number 2 12

pDay ∷ Parser Int
pDay = number 2 31

euroNumDate ∷ Parser DateTime
euroNumDate = do
  d ← pDay
  char '.'
  m ← pMonth
  char '.'
  y ← pYear
  return $ date y m d

americanDate ∷ Parser DateTime
americanDate = do
  y ← pYear
  char '/'
  m ← pMonth
  char '/'
  d ← pDay
  return $ date y m d

euroNumDate' ∷ Int → Parser DateTime
euroNumDate' year = do
  d ← pDay
  char '.'
  m ← pMonth
  return $ date year m d

americanDate' ∷ Int → Parser DateTime
americanDate' year = do
  m ← pMonth
  char '/'
  d ← pDay
  return $ date year m d

strDate ∷ Parser DateTime
strDate = do
  d ← pDay
  space
  ms ← many1 letter
  case lookupMonth ms of
    Nothing → fail $ "unknown month: "++ms
    Just m  → do
      space
      y ← pYear
      notFollowedBy $ char ':'
      return $ date y m d

strDate' ∷ Int → Parser DateTime
strDate' year = do
  d ← pDay
  space
  ms ← many1 letter
  case lookupMonth ms of
    Nothing → fail $ "unknown month: "++ms
    Just m  → return $ date year m d

time24 ∷ Parser Time
time24 = do
  h ← number 2 23
  char ':'
  m ← number 2 59
  x ← optionMaybe $ char ':'
  case x of
    Nothing → return $ Time h m 0
    Just _ → do
      s ← number 2 59
      notFollowedBy letter
      return $ Time h m s

ampm ∷ Parser Int
ampm = do
  s ← many1 letter
  case map toUpper s of
    "AM" → return 0
    "PM" → return 12
    _ → fail "AM/PM expected"

time12 ∷ Parser Time
time12 = do
  h ← number 2 12
  char ':'
  m ← number 2 59
  x ← optionMaybe $ char ':'
  s ← case x of
            Nothing → return 0
            Just s' → number 2 59
  optional space
  hd ← ampm
  return $ Time (h+hd) m s

pDate ∷ Int → Parser DateTime
pDate year = do
  date ← choice $ map try $ map ($ year) $ [
                              const euroNumDate,
                              const americanDate,
                              const strDate,
                              strDate',
                              euroNumDate',
                              americanDate']
  optional $ char ','
  s ← optionMaybe space
  case s of
    Nothing → return date
    Just _ → do
      t ← choice $ map try [time12,time24]
      return $ date `addTime` t

dateType ∷ String → DateType
dateType "start" = StartDate
dateType "end"   = EndDate
dateType "deadline" = Deadline
dateType _ = error "unknown date type"

pSpecDate ∷ Int → Parser (DateType, DateTime)
pSpecDate year = do
  tp ← choice $ map string ["start","end","deadline"]
  string ": "
  dt ← pDate year
  return (dateType tp, dt)

pSpecDates ∷ Int → Parser [(DateType, DateTime)]
pSpecDates year = do
  char '('
  pairs ← (pSpecDate year) `sepBy1` (string "; ")
  string ") "
  return pairs

parseDate ∷ Int → String → Either ParseError DateTime
parseDate year s = parse (pDate year) "" s
