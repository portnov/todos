{-# LANGUAGE UnicodeSyntax, DeriveDataTypeable, NoMonomorphismRestriction #-}

module ConstrSet 
  (CSet,
   empty,
   fromList, toList,
   insert, append,
   elemC,
   selectByConstrOf,
   selectByConstrOf'
  ) where

import Data.Generics
import Data.Function (on)
import Data.List (nubBy)
import Data.Maybe (fromMaybe)

teq = (==) `on` toConstr

data (Data a) ⇒ CSet a = CSet [a]

empty = CSet []

fromList lst = CSet $ nubBy teq lst
toList (CSet lst) = lst

insert item (CSet []) = CSet [item]
insert item (CSet lst) = CSet $ nubBy teq (item:lst)

selectByConstrOf (CSet []) _ = Nothing
selectByConstrOf (CSet (x:xs)) y | x `teq` y = Just x
                                 | otherwise = selectByConstrOf (CSet xs) y

selectByConstrOf' def cset x = fromMaybe def $ selectByConstrOf cset x

elemC _ (CSet []) = False
elemC x (CSet (y:ys)) | x `teq` y = True
                      | otherwise = elemC x (CSet ys)

append (CSet xs) (CSet ys) = CSet $ nubBy teq (xs++ys)
