{-# LANGUAGE UnicodeSyntax #-}
-- | This module contains instances of RuntimeConfig class for DefaultConfig and PrintConfig
module Todos.ConfigInstances where

import Todos.Unicode
import Todos.Config
import Todos.CmdLine

instance RuntimeConfig DefaultConfig where
  getPredicate dt conf = compose dt $ query conf
  toBaseConfig = baseConfig

instance (RuntimeConfig c) ⇒ RuntimeConfig (PrintConfig c) where
  getPredicate = const doHighlight
  toBaseConfig = toBaseConfig ∘ printConfig

