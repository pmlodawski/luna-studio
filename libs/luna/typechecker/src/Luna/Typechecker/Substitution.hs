module Luna.Typechecker.Substitution (
    Subst(..), Types(..)
  ) where

import Luna.Typechecker.Type.Type (Type, Tyvar)

import Data.Monoid


newtype Subst = Subst [(Tyvar, Type)]

instance Monoid Subst where
  mempty = Subst []
  mappend ss1@(Subst s1) (Subst s2) = Subst $ [(u, apply ss1 t) | (u, t) <- s2] ++ s1


class Types t where
  apply :: Subst -> t -> t
  tv :: t -> [Tyvar]

instance Types Type where
  apply = error "Substitution.hs : instance Types Type : apply"
  tv    = error "Substitution.hs : instance Types Type : tv"