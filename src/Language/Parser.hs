module Language.Parser where
  import Text.Parsec
  import Text.Parsec.String
  import Text.Parsec.Char

  import Language.Syntax

  import Control.Monad (void)

  skippable :: Parser ()
  skippable = choice [singleLineComment, spaces]

  singleLineComment :: Parser ()
  singleLineComment = do
    string "--"
    manyTill anyChar (try endOfLine)
    return ()

  break :: Parser Statement
  break = do
    start <- getPosition
    string "break"
    end <- getPosition
    return (Break (SourceRange start end))

  statement :: Parser Statement
  statement = choice [Language.Parser.break]

  nil :: Parser Expression
  nil = do
    start <- getPosition
    string "nil"
    end <- getPosition
    return (Nil (SourceRange start end))

  primaryExpression :: Parser Expression
  primaryExpression = choice [nil]

  expression = primaryExpression

  returnStatement :: Parser Return
  returnStatement = do
    start <- getPosition
    string "return"
    skippable
    expr <- expression
    end <- getPosition
    return (Return (SourceRange start end) expr)
