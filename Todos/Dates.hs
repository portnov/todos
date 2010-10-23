{-# LANGUAGE UnicodeSyntax #-}
-- | Operations with dates
module Todos.Dates
  (parseDate, getCurrentDateTime,
   pSpecDates)
  where

import Data.Char (toUpper)
import Data.Function (on)
import Data.List
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.LocalTime
import Text.ParserCombinators.Parsec

import Todos.Types
import Todos.Unicode

getCurrentDateTime ∷  IO DateTime
getCurrentDateTime = do
  zt ← getZonedTime
  let lt = zonedTimeToLocalTime zt
      ld = localDay lt
      ltod = localTimeOfDay lt
      (y,m,d) = toGregorian ld
      h = todHour ltod
      min = todMin ltod
      s = round $ todSec ltod
  return $ DateTime (fromIntegral y) m d h min s

uppercase ∷ String → String
uppercase = map toUpper

isPrefixOfI ∷  String → String → Bool
p `isPrefixOfI` s = (uppercase p) `isPrefixOf` (uppercase s)

lookupS ∷ String → [(String,a)] → Maybe a
lookupS _ [] = Nothing
lookupS k ((k',v):other) | k `isPrefixOfI` k' = Just v
                         | otherwise          = lookupS k other

monthsN ∷ [(String,Int)]
monthsN = zip months [1..]

lookupMonth ∷ String → Maybe Int
lookupMonth n = lookupS n monthsN

date ∷  Int → Int → Int → DateTime
date y m d = DateTime y m d 0 0 0

addTime ∷  DateTime → Time → DateTime
addTime dt t = dt {
                 hour = tHour t + hour dt,
                 minute = tMinute t + minute dt,
                 second = tSecond t + second dt }

times ∷ Int → TParser t → TParser [t]
times 0 _ = return []
times n p = do
  ts ← times (n-1) p
  t ← optionMaybe p
  case t of
    Just t' → return (ts ++ [t'])
    Nothing → return ts
                               
number ∷ Int → Int → TParser Int
number n m = do
  t ← read `fmap` (n `times` digit)
  if t > m
    then fail "number too large"
    else return t

pYear ∷ TParser Int
pYear = do
  y ← number 4 10000
  if y < 2000
    then return (y+2000)
    else return y

pMonth ∷ TParser Int
pMonth = number 2 12

pDay ∷ TParser Int
pDay = number 2 31

euroNumDate ∷ TParser DateTime
euroNumDate = do
  d ← pDay
  char '.'
  m ← pMonth
  char '.'
  y ← pYear
  return $ date y m d

americanDate ∷ TParser DateTime
americanDate = do
  y ← pYear
  char '/'
  m ← pMonth
  char '/'
  d ← pDay
  return $ date y m d

euroNumDate' ∷ Int → TParser DateTime
euroNumDate' year = do
  d ← pDay
  char '.'
  m ← pMonth
  return $ date year m d

americanDate' ∷ Int → TParser DateTime
americanDate' year = do
  m ← pMonth
  char '/'
  d ← pDay
  return $ date year m d

strDate ∷ TParser DateTime
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

strDate' ∷ Int → TParser DateTime
strDate' year = do
  d ← pDay
  space
  ms ← many1 letter
  case lookupMonth ms of
    Nothing → fail $ "unknown month: "++ms
    Just m  → return $ date year m d

time24 ∷ TParser Time
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

ampm ∷ TParser Int
ampm = do
  s ← many1 letter
  case map toUpper s of
    "AM" → return 0
    "PM" → return 12
    _ → fail "AM/PM expected"

time12 ∷ TParser Time
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

pAbsDate ∷ Int → TParser DateTime
pAbsDate year = do
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

data DateIntervalType = Day | Week | Month | Year
  deriving (Eq,Show,Read)

data DateInterval = Days ℤ
                  | Weeks ℤ
                  | Months ℤ
                  | Years ℤ
  deriving (Eq,Show)

convertTo ∷  DateTime → Day
convertTo dt = fromGregorian (fromIntegral $ year dt) (month dt) (day dt)

convertFrom ∷  Day → DateTime
convertFrom dt = 
  let (y,m,d) = toGregorian dt
  in  date (fromIntegral y) m d

modifyDate ∷  (t → Day → Day) → t → DateTime → DateTime
modifyDate fn x dt = convertFrom $ fn x $ convertTo dt

addInterval ∷  DateTime → DateInterval → DateTime
addInterval dt (Days ds) = modifyDate addDays ds dt
addInterval dt (Weeks ws) = modifyDate addDays (ws*7) dt
addInterval dt (Months ms) = modifyDate addGregorianMonthsClip ms dt
addInterval dt (Years ys) = modifyDate addGregorianYearsClip ys dt

maybePlural ∷ String → TParser String
maybePlural str = do
  r ← string str
  optional $ char 's'
  return (capitalize r)

pDateInterval ∷ TParser DateIntervalType
pDateInterval = do
  s ← choice $ map maybePlural ["day", "week", "month", "year"]
  return $ read s

pRelDate ∷ DateTime → TParser DateTime
pRelDate date = do
  offs ← (try futureDate) <|> (try passDate) <|> (try today) <|> (try tomorrow) <|> yesterday
  return $ date `addInterval` offs

futureDate ∷ TParser DateInterval
futureDate = do
  string "in "
  n ← many1 digit
  char ' '
  tp ← pDateInterval
  case tp of
    Day →   return $ Days (read n)
    Week →  return $ Weeks (read n)
    Month → return $ Months (read n)
    Year →  return $ Years (read n)

passDate ∷ TParser DateInterval
passDate = do
  n ← many1 digit
  char ' '
  tp ← pDateInterval
  string " ago"
  case tp of
    Day →   return $ Days $ - (read n)
    Week →  return $ Weeks $ - (read n)
    Month → return $ Months $ - (read n)
    Year →  return $ Years $ - (read n)

today ∷ TParser DateInterval
today = do
  string "today"
  return $ Days 0

tomorrow ∷ TParser DateInterval
tomorrow = do
  string "tomorrow"
  return $ Days 1

yesterday ∷ TParser DateInterval
yesterday = do
  string "yesterday"
  return $ Days (-1)

pDate ∷ DateTime → TParser DateTime
pDate date =  (try $ pRelDate date) <|> (try $ pAbsDate $ year date)

dateType ∷ String → DateType
dateType "start" = StartDate
dateType "end"   = EndDate
dateType "deadline" = Deadline
dateType _ = error "unknown date type"

pSpecDate ∷ DateTime → TParser (DateType, DateTime)
pSpecDate date = do
  tp ← choice $ map string ["start","end","deadline"]
  string ": "
  dt ← pDate date
  return (dateType tp, dt)

pSpecDates ∷ DateTime → TParser [(DateType, DateTime)]
pSpecDates date = do
  char '('
  pairs ← (pSpecDate date) `sepBy1` (string "; ")
  string ") "
  return pairs

-- | Parse date/time
parseDate ∷ Config
          → DateTime  -- ^ Current date/time
          → String    -- ^ String to parse
          → Either ParseError DateTime
parseDate conf date s = runParser (pDate date) conf "" s
