{-# LANGUAGE UnicodeSyntax #-}
module Todos.Unicode where

type ℝ = Float
type ℤ = Integer
type 𝔹 = Bool

-- | Concatenate lists 
(⧺) ∷ [a] → [a] → [a]
(⧺) = (++)

(⋄) = ($)
infixr 0 ⋄

(∘) = (.)
(∨) = (||)
(∧) = (&&)
x ∈ lst = elem x lst
