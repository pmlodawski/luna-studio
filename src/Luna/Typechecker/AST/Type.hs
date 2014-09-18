module Luna.Typechecker.AST.Type (
    Type(..), Tyvar(..), Tycon(..),
    fn, pair, list, tUnit, tChar, tBool, tInt, tInteger, tFloat, tDouble, tList, tArrow, tTuple2, tString, tCons
  ) where

import Luna.Typechecker.AST.Internal.Type (Type(..),Tyvar(..),Tycon(..))

import Luna.Typechecker.AST.Kind          (Kind(..))


tUnit, tChar, tBool, tInt, tInteger, tFloat, tDouble, tList, tArrow, tTuple2, tString :: Type
tUnit    = TCon (Tycon "()"      Star)
tBool    = TCon (Tycon "Bool"    Star)
tChar    = TCon (Tycon "Char"    Star)
tInt     = TCon (Tycon "Int"     Star)
tInteger = TCon (Tycon "Integer" Star)
tFloat   = TCon (Tycon "Float"   Star)
tDouble  = TCon (Tycon "Double"  Star)

tList    = TCon (Tycon "[]"      (Kfun Star Star))
tArrow   = TCon (Tycon "(->)"    (Kfun Star (Kfun Star Star)))
tTuple2  = TCon (Tycon "(,)"     (Kfun Star (Kfun Star Star)))

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
