{-# LANGUAGE CPP #-}
-- | Wrapper to support unicode IO with GHC 6.10 and 6.12
module IO 
#if __GLASGOW_HASKELL__ < 612
  (module System.IO.UTF8)
#else
  (module Prelude)
#endif
  where

#if __GLASGOW_HASKELL__ < 612

   import Prelude hiding (putStr, putStrLn,readFile,getContents,print)
   import System.IO.UTF8

#else

  import Prelude

#endif
