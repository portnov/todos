module QDL where

import Data.Tree
import TodoParser

select pred todos = filter pred $ map (select' pred) todos

select' pred todo@(Node item children) | pred todo = Node item (select pred children)
                                       | otherwise = Node item []
