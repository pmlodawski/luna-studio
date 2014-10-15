module Luna.Typechecker.Type (
    Scheme(..), Type(..), Tyvar(..), Tycon(..),
    tChar, tDouble, tInt, tString,
    mkTyList,
    instantiate
  ) where

import Luna.Typechecker.IDs
import Luna.Typechecker.Type.Type
import Luna.Typechecker.Type.Scheme

tChar, tDouble, tInt, tList, tString :: Type
tChar   = TConst (Tycon (TyID "Char"))
tDouble = TConst (Tycon (TyID "Double"))
tInt    = TConst (Tycon (TyID "Int"))
tString = mkTyList tChar
tList   = TConst (Tycon (TyID "[]"))

mkTyList :: Type -> Type
mkTyList = TAp tList