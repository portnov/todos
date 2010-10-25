{-# LANGUAGE UnicodeSyntax #-}

-- | Module for parsing config files
module Todos.ReadConfig
  (readConfig)
  where

import Prelude hiding (putStrLn,readFile,getContents,print)
import Todos.IO
import System.Environment
import System.FilePath 
import System.Directory (doesFileExist)
import Data.Maybe
import Data.Either
import Text.ParserCombinators.Parsec

import Todos.Unicode
import Todos.Types

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

readFile' ∷ FilePath → IO [String]
readFile' path = 
  do b ← doesFileExist path
     if not b
       then return []
       else do
              str ← readFile path
              return $ parseConfig (unwords $ lines str)

-- | Read list of options from config files
readConfig ∷ IO [String]
readConfig = do
  home ← getEnv "HOME"
  let homepath = home </> ".config" </> "todos"
  homecfg ← readFile' homepath
  localcfg ← readFile' ".todos.conf"
  return $ homecfg ⧺ localcfg
  
