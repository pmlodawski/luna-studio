module Luna.Typechecker.Substitution (
    Subst(..), Types(..),
    removeSubstitution, addSubstitution, addSubstitutions
  ) where

import Luna.Typechecker.IDs
import Luna.Typechecker.Type.Type

import Data.List
import Data.Maybe
import Data.Monoid



newtype Subst = Subst [(Tyvar, Type)]

instance Monoid Subst where
  mempty = Subst []
  mappend ss1@(Subst s1) (Subst s2) = Subst $ [(u, apply ss1 t) | (u, t) <- s2] ++ s1



removeSubstitution :: TyID -> Subst -> Subst
removeSubstitution = undefined


addSubstitution :: Tyvar -> Type -> Subst -> Subst
addSubstitution tyvar t = mappend $ Subst [(tyvar, t)]

addSubstitutions :: [(Tyvar, Type)] -> Subst -> Subst
addSubstitutions ts s = mconcat $ map (\x -> Subst [x]) ts ++ [s]



class Types t where
  apply :: Subst -> t -> t
  ftv :: t -> [Tyvar]

instance Types Type where
  apply (Subst s) (TVar tyv) = fromMaybe (TVar tyv) (lookup tyv s)
  apply s t = t
  ftv (TVar tyv) = [tyv]
  ftv (TConst _) = []
  ftv (TAp t1 t2) = ftv t1 `union` ftv t2

instance (Types a) => Types [a] where
  apply = error "Substitution.hs : instance Types [a] : apply"
  ftv = nub . concatMap ftv