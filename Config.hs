{-# LANGUAGE UnicodeSyntax #-}

module Config (readConfig) where

import System.Environment
import System.FilePath 
import System.Directory (doesFileExist)
import Data.Maybe
import Data.Either
import Data.ConfigFile

import Unicode
import Types
import ConstrSet (fromList)

getVal conf name = either (const Nothing) Just $ get conf "DEFAULT" name

readfile' conf path = 
  do b <- doesFileExist path
     if not b
       then return conf
       else do
               ec <- readfile conf path
               return $ case ec of
                         Left _ -> conf
                         Right c -> c

readConfig :: IO (String, String, Options)
readConfig = do
  home <- getEnv "HOME"
  c1 <- readfile' emptyCP (home </> ".config" </> "todos")
  config <- readfile' c1 ".todos.conf"
  let prune = getVal config "prune"
      start = getVal config "minLevel"
      exec  = getVal config "execute"
      prefix  = getVal config "prefix"
      defPrefix = getVal config "default-prefix"
      defExec   = getVal config "default-execute"
      lflags = catMaybes [Prune `fmap` prune, Start `fmap` start] 
      mflags = catMaybes [Execute `fmap` exec, Prefix `fmap` prefix]
  return (fromMaybe "" defPrefix,
          fromMaybe "" defExec,
          O [] (fromList mflags) (fromList lflags))

