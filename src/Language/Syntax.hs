module Language.Syntax where
  import Text.Parsec

  data SourceRange = SourceRange {
    start  :: SourcePos,
    end :: SourcePos
  } deriving (Show, Eq)

  data Statement =
    Break SourceRange
    deriving (Show, Eq)

  data Return = Return SourceRange Expression deriving (Show, Eq)

  data Expression =
    Nil SourceRange
    | Boolean SourceRange Bool
    | Number SourceRange String
    | String SourceRange String

    | BinaryOperation SourceRange BinaryOperator Expression Expression
    deriving (Show, Eq)

  data BinaryOperator = Add | Sub | Mul | Div | Exp | Mod | Concat
    | LT | LTE | GT | GTE | EQ | NEQ | And | Or
    deriving (Show, Eq)

  data Block = Block SourceRange [Statement] (Maybe Return) deriving (Show, Eq)
