module Main where

import System.Environment
import Language.ExtLua.Parser
import Language.ExtLua.Printer

main :: IO ()
main = do
  [file] <- getArgs
  result <- parseFile file
  case result of
    Right block -> print $ pprint block
    Left _ -> print "Error"
