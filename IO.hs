{-# LANGUAGE CPP #-}
module IO 
  (putStrLn,readFile,getContents,print)
  where

#if __GLASGOW_HASKELL__ < 612

   import Prelude hiding (putStrLn,readFile,getContents,print)
   import System.IO.UTF8

#else

  import Prelude (putStrLn,readFile,getContents,print)

#endif
