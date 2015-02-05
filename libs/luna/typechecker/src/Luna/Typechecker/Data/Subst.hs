module Luna.Typechecker.Data.Subst where


import            Flowbox.Prelude

import            Data.Sequence              as Seq
-- import            Data.Packable              (Pack(pack), Unpack(unpack), Packable)

import            Luna.Typechecker.Data.TVar
import            Luna.Typechecker.Data.Type


newtype Subst = Subst { fromSubst :: Seq (TVar, Type) }


makePrisms ''Subst


-- instance Unpack Subst [(TVar, Type)]    where unpack = ICMap.toList . fromSubst
-- instance Pack [(TVar, Type)] Subst      where pack   = Subst . ICMap.fromList
-- instance Packable Subst [(TVar, Type)]


instance Default Subst where
  def = identitySubst


singleSubst :: TVar -> Type -> Subst
singleSubst x t = Subst $ Seq.singleton (x, t)


identitySubst :: Subst
identitySubst = Subst Seq.empty
