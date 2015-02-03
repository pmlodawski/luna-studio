module Luna.Typechecker.Data.Subst where


import            Flowbox.Prelude

import            Data.Map.IntConvertibleMap (IntConvertibleMap)
import qualified  Data.Map.IntConvertibleMap as ICMap
import            Data.Packable              (Pack(pack), Unpack(unpack), Packable)

import            Luna.Typechecker.Data.TVar
import            Luna.Typechecker.Data.Type


newtype Subst = Subst { fromSubst :: IntConvertibleMap TVar Type }

makePrisms ''Subst


instance Unpack Subst [(TVar, Type)]    where unpack = ICMap.toList . fromSubst
instance Pack [(TVar, Type)] Subst      where pack   = Subst . ICMap.fromList
instance Packable Subst [(TVar, Type)]


instance Default Subst where
  def = Subst $ ICMap.empty


singleSubst :: TVar -> Type -> Subst
singleSubst x t = Subst $ ICMap.singleton x t


identitySubst :: Subst
identitySubst = def
