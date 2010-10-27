{-# LANGUAGE UnicodeSyntax #-}
module Todos.Unicode where

type ℝ = Float
type ℤ = Integer
type 𝔹 = Bool

-- | Concatenate lists 
(⧺) ∷ [a] → [a] → [a]
(⧺) = (++)

(⋄) ∷ (a -> b) -> a -> b
(⋄) = ($)
infixr 0 ⋄

(∘) :: (b -> c) -> (a -> b) -> a -> c
(∘) = (.)

(∨) :: Bool -> Bool -> Bool
(∨) = (||)

(∧) :: Bool -> Bool -> Bool
(∧) = (&&)

(∈) :: (Eq a) => a -> [a] -> Bool
x ∈ lst = elem x lst

