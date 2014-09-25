module Luna.Typechecker.Substitutions (
    Subst, Types(..), (@@), merge, nullSubst, (+->)
  ) where


import Luna.Typechecker.AST.Type        (Tyvar(..),Type(..))

import Luna.Typechecker.Internal.Logger

import Data.List                        (intersect,nub,union)
import Data.Maybe                       (fromMaybe)


type Subst = [(Tyvar, Type)]


class Types t where
  apply :: Subst -> t -> t
  tv :: t -> [Tyvar]


instance Types Type where
  apply s (TVar u)  = fromMaybe (TVar u) (lookup u s)
  apply s (TAp l r) = TAp (apply s l) (apply s r)
  apply _ t         = t
  tv (TVar u)  = [u]
  tv (TAp l r) = tv l `union` tv r
  tv _         = []

instance Types a => Types [a] where
  apply s = map (apply s)
  tv = nub . concatMap tv


nullSubst :: Subst
nullSubst = []

(+->) :: Tyvar -> Type -> Subst
u +-> t = [(u, t)]

infixr 4 @@
(@@) :: Subst -> Subst -> Subst
s1 @@ s2 = [(u, apply s1 t) | (u, t) <- s2] ++ s1

merge :: (Monad m) => Subst -> Subst -> TCLoggerT m Subst
merge s1 s2 = if agree then return (s1 ++ s2) else throwError "merge fails"
  where agree = all (\v -> apply s1 (TVar v) == apply s2 (TVar v))
                    (map fst s1 `intersect` map fst s2)
