{-# LANGUAGE UnicodeSyntax #-}

module Config (readConfig) where

import System.Environment
import System.FilePath 
import System.Directory (doesFileExist)
import Data.Maybe
import Data.Either

import Unicode
import Types

readFile' ∷ FilePath → IO String
readFile' path = 
  do b <- doesFileExist path
     if not b
       then return ""
       else readFile path

readConfig :: IO [String]
readConfig = do
  home <- getEnv "HOME"
  let homepath = home </> ".config" </> "todos"
  homecfg ← return ∘ words =<< readFile' homepath
  localcfg ← return ∘ words =<< readFile' ".todos.conf"
  return $ homecfg ⧺ localcfg
  
