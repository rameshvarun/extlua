module Language.Parser where
  import Text.Parsec
  import Text.Parsec.String
  import Text.Parsec.Char

  import Language.Syntax

  import Control.Monad (void)

  whitespace :: Parser ()
  whitespace = void $ many $ oneOf " \n\t\r\f"

  breakStatement :: Parser Statement
  breakStatement = do
    start <- getPosition
    string "break"
    end <- getPosition
    return BreakStatement (SourceRange start end)
