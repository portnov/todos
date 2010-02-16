{-# LANGUAGE UnicodeSyntax #-}

module CommandParser where

import Unicode
import Types

-- | Format item info
printfItem ∷ String      -- ^ Format string
           → TodoItem 
           → String
printfItem pattern item = printf pattern
  where
    printf "" = ""
    printf [x] = [x]
    printf ('%':c:xs) = (itemPart c) ⧺ printf xs
    printf (x:xs) = x:printf xs

    itemPart 'L' = show $ itemLevel item
    itemPart 'n' = itemName item
    itemPart 't' = unwords $ itemTags item
    itemPart 's' = itemStatus item
    itemPart 'd' = itemDescr item
    itemPart 'f' = fileName item
    itemPart 'l' = show $ lineNr item
    itemPart x   = [x]
