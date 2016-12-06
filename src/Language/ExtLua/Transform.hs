import Language.ExtLua.Syntax
import Language.ExtLua.Lexer (SourcePos(..), SourceRange(..))

assignOpToBinOp :: Assignop a -> Binop a
assignOpToBinOp (AddEquals a) = Add a

transformAssignOp :: Stat SourceRange -> Stat SourceRange
transformAssignOp (Assignop sr op names exprs) = Assign sr names exprs
transformAssignOp a = a
