module Language.Syntax where
  import Text.Parsec

  data SourceRange = SourceRange {
    start  :: SourcePos,
    end :: SourcePos
  } deriving (Show, Eq)

  data Statement =
    Break SourceRange

  data ReturnStatement = ReturnStatement SourceRange Expression

  data Expression =
    Nil SourceRange

  data Block = Block SourceRange [Statement] (Maybe ReturnStatement)
