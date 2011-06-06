{-# LANGUAGE UnicodeSyntax #-}

module Todos.CommandParser where

import Todos.Unicode
import Todos.Types

-- | Format item info
printfItem ∷ String      -- ^ Format string
           → TodoItem 
           → String
printfItem pattern item = printf pattern
  where
    printf ""          = ""
    printf [x]         = [x]
    printf ('%':c:xs)  = itemPart c ⧺ printf xs
    printf ('\\':c:xs) = escape c : printf xs
    printf (x:xs)      = x:printf xs

    escape '\\' = '\\'
    escape 't'  = '\t'
    escape 'n'  = '\n'
    escape 'b'  = '\b'
    escape 'v'  = '\v'
    escape c    = c

    itemPart 'n' = itemName item
    itemPart 't' = unwords $ itemTags item
    itemPart 's' = itemStatus item
    itemPart 'p' = itemPrefix item
    itemPart 'd' = itemDescr item
    itemPart 'f' = fileName item
    itemPart 'i' = makeId item
    itemPart 'l' = show $ lineNr item
    itemPart 'D' | null dates = ""
                 | otherwise  = "(" ⧺ dates ⧺ ") "
    itemPart x   = [x]

    dates = showDates [StartDate `is` startDate item, EndDate `is` endDate item, Deadline `is` deadline item]

