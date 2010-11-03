{-# LANGUAGE UnicodeSyntax #-}

module Todos.Default.Config
  (module Todos.Config,
   DefaultConfig (..)
  ) where

import Todos.Types
import Todos.Config

-- | Default runtime configuration type. Is read from command line and configs.
data DefaultConfig = DConfig {
      baseConfig ∷ BaseConfig,
      query ∷ Composed }
    deriving (Eq,Show)

