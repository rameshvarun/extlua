module Language.ExtLua.Transform where
  import Language.ExtLua.Syntax
  import Language.ExtLua.Lexer (SourcePos(..), SourceRange(..))
  import Data.Text (pack)

  assignOpToBinOp :: Assignop a -> Binop a
  assignOpToBinOp (AddEquals a) = Add a
  assignOpToBinOp (SubEquals a) = Sub a

  transformExpression :: Exp SourceRange -> Exp SourceRange
  transformExpression e = e

  transformPrefixExpression :: PrefixExp SourceRange -> PrefixExp SourceRange
  transformPrefixExpression (PEFunCall sr funcall) = PEFunCall sr $ transformFunctionCall funcall
  transformPrefixExpression pe = pe

  transformStatement :: Stat SourceRange -> Stat SourceRange
  transformStatement (Assignop sr assignop names exprs) = Assign sr names
    [Binop sr (assignOpToBinOp assignop) (PrefixExp sr (PEVar sr name)) expr | (expr, name) <- zip exprs names]
  transformStatement (FunCall src funcall) = FunCall src (transformFunctionCall funcall)
  transformStatement a = a

  transformFunctionCall :: FunCall SourceRange -> FunCall SourceRange
  transformFunctionCall (BoundFunctionCall sr prefix name arg) = NormalFunCall sr (PEVar sr (VarName sr name)) newarg
    where newarg = case arg of
                      Args src expr -> Args src (PrefixExp src (transformPrefixExpression prefix) : expr)
  transformFunctionCall a = a

  transformBlock :: Block SourceRange -> Block SourceRange
  transformBlock (Block src stmnts e) = Block src (map transformStatement stmnts) e
