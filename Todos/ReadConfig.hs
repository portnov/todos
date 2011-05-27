{-# LANGUAGE UnicodeSyntax #-}

-- | Module for parsing config files
module Todos.ReadConfig
  (readAllConfigs, readConfigFile)
  where

import Prelude hiding (putStrLn,readFile,getContents,print)
import Todos.IO
import System.Environment
import System.FilePath 
import System.Directory (doesFileExist, getCurrentDirectory)
import Text.ParserCombinators.Parsec

import Todos.Unicode

word ∷ Parser String
word = choice $ map try [quotedOption, simpleOption, quoted, simpleWord]

simpleWord = many1 $ noneOf " \t\r\n=\"'"

quotedOption = (try quotedLongOption) <|> quotedShortOption

quotedLongOption ∷ Parser String
quotedLongOption = do
  string "--"
  o ← simpleWord
  char '='
  v ← quoted
  return ("--" ⧺ o ⧺ "=" ⧺ v)

quotedShortOption ∷ Parser String
quotedShortOption = do
  string "-"
  o ← simpleWord
  v ← quoted
  return ("-" ⧺ o ⧺ v)

simpleOption = do
  o ← simpleWord
  optional $ char '='
  v ← simpleWord
  return (o ⧺ "=" ⧺ v)

quoted = quoted1 <|> quoted2

quoted1 = do
  char '\''
  s ← many1 $ noneOf "'"
  char '\''
  return s

quoted2 = do
  char '"'
  s ← many1 $ noneOf "\""
  char '"'
  return s

pConfig ∷ Parser [String]
pConfig = word `sepBy` space

parseConfig ∷ String → [String]
parseConfig str = 
  case parse pConfig "config file" str of
    Right lst → lst
    Left err → error $ show err

-- | Read list of options from given config file
readConfigFile ∷ FilePath → IO [String]
readConfigFile path = 
  do b ← doesFileExist path
     if not b
       then return []
       else do
              str ← readFile path
              return $ parseConfig (unwords $ lines str)

readFiles ∷ [FilePath] → IO [String]
readFiles [] = return []
readFiles (path:other) = do
  content ← readConfigFile path
  case content of
    "%":options → do otherOptions ← readFiles other
                     return $ otherOptions ⧺ options
    []          → readFiles other
    _           → return content

-- | Read list of options from config files
readAllConfigs ∷ IO [String]
readAllConfigs = do
  home ← getEnv "HOME"
  let homepath = home </> ".config" </> "todos" </> "todos.conf"
  homecfg ← readConfigFile homepath
  pwd <- getCurrentDirectory
  localcfg ← readFiles $ map (</> ".todos.conf") $ scanl1 (</>) $ splitPath pwd
  return $ homecfg ⧺ localcfg
  
