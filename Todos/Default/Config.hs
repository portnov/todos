{-# LANGUAGE UnicodeSyntax #-}
-- | This module contains declaration of DefaultConfig data type, which is used
-- by default to store runtime config.
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

