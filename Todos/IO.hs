{-# LANGUAGE CPP #-}
-- | Wrapper to support unicode IO with GHC 6.10 and 6.12
module Todos.IO 
  (putStr, putStrLn,readFile,getContents,print,
   encodeString, decodeString)
  where

#if __GLASGOW_HASKELL__ < 612

   import Prelude hiding (putStr, putStrLn,readFile,getContents,print)
   import System.IO.UTF8
   import Codec.Binary.UTF8.String (encodeString, decodeString)

#else

  import Prelude

  encodeString :: String -> String
  encodeString = id

  decodeString :: String -> String
  decodeString = id

#endif
