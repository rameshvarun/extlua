module Main where

import System.Environment
import Language.ExtLua.Parser

main :: IO ()
main = do
  [file] <- getArgs
  result <- parseFile file
  print $ show result
