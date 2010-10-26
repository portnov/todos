{-# LANGUAGE UnicodeSyntax #-}
module Todos.ConfigInstances where

import Todos.Unicode
import Todos.Types
import Todos.Config
import Todos.CmdLine

instance QueryConfig Config where
  getPredicate dt conf = compose dt $ query conf
  toBaseConfig = baseConfig

instance (QueryConfig c) ⇒ QueryConfig (PrintConfig c) where
  getPredicate = const doHighlight
  toBaseConfig = toBaseConfig ∘ printConfig

