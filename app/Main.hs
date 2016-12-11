module Main where

import System.Environment
import Language.ExtLua.Parser
import Language.ExtLua.Printer
import Language.ExtLua.Transform

main :: IO ()
main = do
  [file] <- getArgs
  result <- parseFile file
  case result of
    Right block -> print $ pprint $ transformBlock block
    Left _ -> print "Error"
