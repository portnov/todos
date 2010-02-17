{-# LANGUAGE CPP #-}
-- | Wrapper to support unicode IO with GHC 6.10 and 6.12
module IO 
  (putStrLn,readFile,getContents,print)
  where

#if __GLASGOW_HASKELL__ < 612

   import Prelude hiding (putStrLn,readFile,getContents,print)
   import System.IO.UTF8

#else

  import Prelude (putStrLn,readFile,getContents,print)

#endif
