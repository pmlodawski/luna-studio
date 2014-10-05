module Luna.Typechecker.AST.Type (
    Type(..), Tyvar(..), Tycon(..),
    fn, pair, list, tUnit, tChar, tBool, tInt, tInteger, tFloat, tDouble, tList, tArrow, tTuple2, tString, tCons
  ) where


import Luna.Typechecker.AST.Kind          (Kind(..))
import Luna.Typechecker.AST.TID           (TID(..))

import Luna.Typechecker.AST.Internal.Type (Type(..),Tyvar(..),Tycon(..))


tUnit, tChar, tBool, tInt, tInteger, tFloat, tDouble, tList, tArrow, tTuple2, tString :: Type
tUnit    = TCon (Tycon (TID "()"     ) Star)
tBool    = TCon (Tycon (TID "Bool"   ) Star)
tChar    = TCon (Tycon (TID "Char"   ) Star)
tInt     = TCon (Tycon (TID "Int"    ) Star)
tInteger = TCon (Tycon (TID "Integer") Star)
tFloat   = TCon (Tycon (TID "Float"  ) Star)
tDouble  = TCon (Tycon (TID "Double" ) Star)
tList    = TCon (Tycon (TID "[]"     ) (Kfun Star Star))
tArrow   = TCon (Tycon (TID "(->)"   ) (Kfun Star (Kfun Star Star)))
tTuple2  = TCon (Tycon (TID "(,)"    ) (Kfun Star (Kfun Star Star)))
tString  = list tChar

tCons :: Type -> Type
tCons  a = a `fn` list a `fn` list a

infixr 4 `fn`
fn :: Type -> Type -> Type
a `fn` b = TAp (TAp tArrow a) b

list :: Type -> Type
list = TAp tList

pair :: Type -> Type -> Type
pair = TAp . TAp tTuple2
