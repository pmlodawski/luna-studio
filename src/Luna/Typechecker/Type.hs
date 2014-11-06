module Luna.Typechecker.Type (
    Scheme(..), Type(..), Tyvar(..), Tycon(..),
    tChar, tDouble, tInt, tString,
    mkTyList, mkTyFun,
    instantiate
  ) where

import Luna.Typechecker.IDs         (TyID(..))
import Luna.Typechecker.Type.Type   (Type(..),Tyvar(..),Tycon(..))
import Luna.Typechecker.Type.Scheme (Scheme(..),instantiate)



tChar, tDouble, tInt, tString :: Type
--tChar   = TConst (Tycon (TyID "Char"))
tChar = error "Typechecker.Type:tChar not implemented"
--tDouble = TConst (Tycon (TyID "Double"))
tDouble = error "Typechecker.Type:tDouble not implemented"
--tInt    = TConst (Tycon (TyID "Int"))
tInt = error "Typechecker.Type:tInt not implemented"
--tString = mkTyList tChar
tString = error "Typechecker.Type:tString not implemented"


tList, tFun :: Type
--tList = TConst (Tycon (TyID "[]"))
tList = error "Typechecker.Type:tList not yet implemented"
--tFun  = TConst (Tycon (TyID "(->)"))
tFun = error "Typechecker.Type:tFun not yet implemented"

mkTyList :: Type -> Type
--mkTyList = TAp tList
mkTyList = error "Typechecker.Type:mkTyList not yet implemented"

mkTyFun :: Type -> Type -> Type
--mkTyFun = TAp . TAp tFun
mkTyFun = error "Typechecker.Type:mkTyFun not yet implemented"