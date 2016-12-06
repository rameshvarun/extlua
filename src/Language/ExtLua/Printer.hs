{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleInstances #-}

#ifndef MIN_VERSION_base
#define MIN_VERSION_base(x,y,z) 1
#endif

-- | Lua pretty-printer.
module Language.ExtLua.Printer
  ( pprint
  , renderPretty
  , displayS
  , displayIO
  , LPretty
  ) where

#if MIN_VERSION_base(4,8,0)
import           Prelude                 hiding (EQ, GT, LT, (<$>))
#else
import           Prelude                 hiding (EQ, GT, LT)
#endif

import qualified Data.Text               as Text

import           Text.PrettyPrint.Leijen hiding ((<$>))

import           Language.ExtLua.Syntax

intercalate :: Doc -> [Doc] -> Doc
intercalate s elems = sep (punctuate s elems)

infixr 5 <$>
(<$>) :: Doc -> Doc -> Doc
x <$> y | isEmpty y = x
        | otherwise = x <> line <> y

type Precedence = Int

class LPretty a where
    pprint :: a -> Doc
    pprint = pprint' 0

    pprint' :: Precedence -> a -> Doc
    pprint' _ = pprint

instance LPretty [Char] where
    pprint = text

instance LPretty Bool where
    pprint True  = text "true"
    pprint False = text "false"

instance LPretty (FunDef a) where
    pprint (FunDef a FunBody{}) = pprint FunBody{}

instance LPretty (Table a) where

instance LPretty (Exp a) where
    pprint' _ (Nil _)            = text "nil"
    pprint' _ (Bool _ s)       = pprint s
    pprint' _ (Number _ _ n)   = text (Text.unpack n)
    pprint' _ (String _ s)     = text (Text.unpack s)
    pprint' _ (Vararg _)         = text "..."
    pprint' _ (EFunDef _ f)    = pprint f
    pprint' _ (PrefixExp _ pe) = pprint pe
    pprint' _ (TableConst _ t) = pprint t
    pprint' p (Binop _ op e1 e2) = ps (pprint' opPrecL e1 <+> pprint op
                                                        <+> case e2 of
                                                              Unop{} -> pprint e2
                                                              _ -> pprint' opPrecR e2)
      where
        (opPrecL, opPrecR) = getBinopPrec op
        ps = if min opPrecL opPrecR < p then parens else id

    -- We handle this as a special case: When we have a chain of negations, we
    -- should put a space between operators, otherwise we end up printing a
    -- comment.
    --
    -- One another solution would be to always put a space after negation, but I
    -- like to put negation just before the expression, without any spaces.
    pprint' p (Unop a (Neg b) (Unop c (Neg d) e)) =
        ps (pprint (Neg b) <+> pprint' opPrec (Unop c (Neg d) e))
      where
        opPrec = getUnopPrec (Neg b)
        ps = if opPrec < p then parens else id

    pprint' p (Unop _ op e)    = ps (pprint op <> pprint' opPrec e)
      where
        opPrec = getUnopPrec op
        ps = if opPrec < p then parens else id

instance LPretty (Var a) where
    pprint (VarName _ n)          = pprint n
    pprint (Select _ pe e)        = pprint pe <> align (brackets (pprint e))
    pprint (SelectName _ pe name) = pprint pe <//> (char '.' <> pprint name)

instance LPretty (Binop a) where
    pprint (Add _)     = char '+'
    pprint (Sub _)     = char '-'
    pprint (Mul    _ ) = char '*'
    pprint (Div    _ ) = char '/'
    pprint (IDiv   _ ) = text "//"
    pprint (Exp    _ ) = char '^'
    pprint (Mod    _ ) = char '%'
    pprint (Concat _ ) = text ".."
    pprint (LT     _ ) = char '<'
    pprint (LTE    _ ) = text "<="
    pprint (GT     _ ) = char '>'
    pprint (GTE    _ ) = text ">="
    pprint (EQ     _ ) = text "=="
    pprint (NEQ    _ ) = text "~="
    pprint (And    _ ) = text "and"
    pprint (Or     _ ) = text "or"
    pprint (BAnd   _ ) = char '&'
    pprint (BOr    _ ) = char '|'
    pprint (BXor   _ ) = char '~'
    pprint (ShiftL _ ) = text "<<"
    pprint (ShiftR _ ) = text ">>"

instance LPretty (Unop a) where
    pprint (Neg _)        = char '-'
    pprint (Not _)        = text "not "
    pprint (Len _)        = char '#'
    pprint (Complement _) = char '~'

getBinopPrec :: (Binop a) -> (Precedence, Precedence)
getBinopPrec op =
    case op of
      (Add _)    -> (10, 10)
      (Sub _)    -> (10, 10)
      (Mul _)    -> (11, 11)
      (Div _)    -> (11, 11)
      (IDiv _)   -> (11, 11)
      (Exp _)    -> (14, 13)
      (Mod _)    -> (11, 11)
      (Concat _) -> (9, 8)
      (ShiftL _) -> (7, 7)
      (ShiftR _) -> (7, 7)
      (BAnd _)   -> (6, 6)
      (BXor _)   -> (5, 5)
      (BOr _)    -> (4, 4)
      (LT _)     -> (3, 3)
      (LTE _)    -> (3, 3)
      (GT _)     -> (3, 3)
      (GTE _)    -> (3, 3)
      (EQ _)     -> (3, 3)
      (NEQ _)    -> (3, 3)
      (And _)    -> (2, 2)
      (Or _)     -> (1, 1)

getUnopPrec :: Unop a -> Precedence
getUnopPrec = const 12

instance LPretty (PrefixExp a) where
    pprint (PEVar _ var)         = pprint var
    pprint (PEFunCall _ funcall) = pprint funcall
    pprint (Paren _ e)           = parens (pprint e)

instance LPretty [TableField a] where
    pprint fields = braces (align (fillSep (punctuate comma (map pprint fields))))

instance LPretty (TableField a) where
    pprint (ExpField _ e1 e2)    = brackets (pprint e1) <+> equals <+> pprint e2
    pprint (NamedField _ name e) = pprint name <+> equals <+> pprint e
    pprint (Field _ e)           = pprint e

instance LPretty (Block a) where
    pprint (Block _ stats ret) =
      case stats of
        [] -> ret'
        _  -> vsep (map pprint stats) <$> ret'
      where ret' = case ret of
                     Nothing -> empty
                     Just [fun@EFunDef{}] -> text "return" <+> pprint fun
                     Just e  -> nest 2 (text "return" </> intercalate comma (map (align . pprint) e))

instance LPretty (FunName a) where
    pprint (FunName _ name s methods) = cat (punctuate dot (map pprint $ name:s)) <> method'
      where method' = case methods of
                        Nothing -> empty
                        Just m' -> char ':' <> pprint m'

instance LPretty (FunBody a) where
    pprint = pprintFunction Nothing

pprintFunction :: Maybe Doc -> (FunBody a) -> Doc
pprintFunction funname (FunBody _ args vararg block) =
    group (nest 2 (header <$> body) <$> end)
  where
    header = case funname of
               Nothing -> text "function" <+> args'
               Just n  -> text "function" <+> n <> args'
    vararg' = case vararg of
                Just a  -> [text "..."]
                Nothing -> []
    args' = parens (align (cat (punctuate (comma <> space) (map pprint args ++ vararg'))))
    body = pprint block
    end = text "end"

instance LPretty (FunCall a) where
    pprint (NormalFunCall _ pe arg)     = pprint pe <> pprint arg
    pprint (MethodCall _ pe method arg) = pprint pe <//> colon <> pprint method <> pprint arg

instance LPretty (FunArg a) where
    pprint (Args _ [fun@EFunDef{}]) = parens (pprint fun)
    pprint (Args _ exps)   = parens (align (fillSep (punctuate comma (map (align . pprint) exps))))
    pprint (TableArg _ t)  = pprint t
    pprint (StringArg _ s) = text (Text.unpack s)

instance LPretty (Stat a) where
    pprint (Assign _ names vals)
        =   intercalate comma (map pprint names)
        <+> equals
        <+> intercalate comma (map pprint vals)
    pprint (FunCall _ funcall) = pprint funcall
    pprint (Label _ name)      = text "::" <> pprint name <> text "::"
    pprint (Break _)             = text "break"
    pprint (Goto _ name)       = text "goto" <+> pprint name
    pprint (Do _ block)        = group (nest 2 (text "do" <$> pprint block) <$> text "end")
    pprint (While _ guard e)
        =  nest 2 (text "while" <+> pprint guard <+> text "do" <$> pprint e)
       <$> text "end"
    pprint (Repeat _ block guard)
        =   nest 2 (text "repeat" <$> pprint block)
        </> nest 2 (text "until" </> pprint guard)

    pprint (If _ cases elsePart) = group (printIf cases elsePart)
      where
        printIf ((guard, block) : xs) e =
          nest 2 (text "if" <+> pprint guard <+> text "then" <$> pprint block) <$> printIf' xs e
        printIf [] _ =
          error $ "pprint: Trying to print invalid syntax:\n\t" ++
                  "if statement should have at least one case"

        printIf' [] Nothing  = text "end"
        printIf' [] (Just b) = nest 2 (text "else" <$> pprint b) <$> text "end"
        printIf' ((guard, block) : xs) e =
          nest 2 (text "elseif" <+> pprint guard <+> text "then" <$> pprint block) <$> printIf' xs e

    pprint (ForRange _ name e1 e2 e3 block)
        =   nest 2 (text "for" <+> pprint name <> equals <> pprint e1
                      <> comma <> pprint e2 <> e3' <+> text "do"
                      <$> pprint block)
        <$> text "end"
      where e3' = case e3 of
                    Nothing -> empty
                    Just e  -> comma <> pprint e

    pprint (ForIn _ names exps block)
        =   nest 2 (text "for" <+> intercalate comma (map pprint names) <+> text "in"
                     <+> intercalate comma (map pprint exps) <+> text "do"
                     <$> pprint block)
        <$> text "end"

    pprint (FunAssign _ name body) = pprintFunction (Just (pprint name)) body
    pprint (LocalFunAssign _ name body) = text "local" <+> pprintFunction (Just (pprint name)) body
    pprint (LocalAssign _ names exps)
        = text "local" <+> intercalate comma (map pprint names) <+> exps'
      where exps' = case exps of
                      Nothing -> empty
                      Just es -> equals </> intercalate comma (map pprint es)
    pprint (EmptyStat _) = text ";"

instance LPretty (Name a) where
  pprint (Name _ n) = text (Text.unpack n)
