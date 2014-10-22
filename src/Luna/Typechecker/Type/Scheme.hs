module Luna.Typechecker.Type.Scheme (
    Scheme(..),
    instantiate
  ) where

import Luna.Typechecker.IDs
import Luna.Typechecker.Substitution
import Luna.Typechecker.TIMonad
import Luna.Typechecker.Type.Type
import Luna.Typechecker.RefactorMePlease

import Data.Monoid



data Scheme = Scheme [TyID] Type


instance Types Scheme where
  apply s (Scheme vars t) = Scheme vars (apply s' t)
    where s' = foldr removeSubstitution s vars -- avoid var-catching substitution!
  ftv   = error "Type.Scheme : Types Scheme : ftv"

instance Show Scheme where
  show (Scheme _ _) = "<scheme>"


instantiate :: Scheme -> TILogger Type
instantiate (Scheme vars t) = do nvars <- mapM (const mkTyID) vars
                                 let s = zipWith (\t1 t2 -> Subst [(Tyvar t1,t2)]) vars nvars
                                 return $ apply (mconcat s) t
