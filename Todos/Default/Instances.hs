{-# LANGUAGE UnicodeSyntax #-}
-- | This module contains instances of RuntimeConfig class for DefaultConfig and PrintConfig
module Todos.Default.Instances where

import Todos.Unicode
import Todos.Config
import Todos.Default.Config
import Todos.Default.CmdLine

instance RuntimeConfig DefaultConfig where
  getPredicate dt conf = compose dt $ query conf
  toBaseConfig = baseConfig

instance (RuntimeConfig c) ⇒ RuntimeConfig (PrintConfig c) where
  getPredicate = const doHighlight
  toBaseConfig = toBaseConfig ∘ printConfig

