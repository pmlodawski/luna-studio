module Luna.Typechecker.Internal.TIMonad (TI(..), newTVar, freshInst, unify, getSubst, runTI, Instantiate(..)) where

import           Luna.Typechecker.Internal.AST.Kind         (Kind)
import           Luna.Typechecker.Internal.AST.Scheme       (Scheme(..))
import           Luna.Typechecker.Internal.AST.TID          (enumTID)
import           Luna.Typechecker.Internal.AST.Type         (Type(..), Tyvar(..))

import           Luna.Typechecker.Internal.Substitutions    (Types(..),Subst,nullSubst,(@@))
import           Luna.Typechecker.Internal.Typeclasses      (Qual(..),Pred(..))
import           Luna.Typechecker.Internal.Unification      (mgu)

import           Control.Applicative                        (Applicative(..))


-- TODO [kgdk] 19 sie 2014: a może użyć State/ST zamiast newtype TI a?
-- TODO [kgdk] 19 sie 2014: zbadać performance 'State (x,y)' vs to tutaj vs 'StateT x (State y)'
-- TODO [kgdk] 21 sie 2014: zrobic z tego Applicative
newtype TI a = TI (Subst -> Int -> (Subst, Int, a))


instance Monad TI where
  return x   = TI (\s n -> (s,n,x))
  TI f >>= g = TI (\s n -> case f s n of
                             (s',n',x) -> let TI gx = g x
                                           in gx s' n')

instance Functor TI where
  --fmap :: (a -> b) -> f a -> f b
  fmap = undefined 

instance Applicative TI where
  --pure :: a -> TI a
  pure = undefined
  --(<*>) :: f (a -> b) -> f a -> f b
  (<*>) = undefined


-- TODO [kgdk] 19 sie 2014: co to jest z tymi nazwami? :< to powinno być nazwą pola rekordu natomiast
-- to co tutaj to powinno się nazywać… runTIWithDefault lub podobnie oraz getTI
-- TODO [kg] 15 lip 2014: opcjonalnie zrobić instance MonadState TI
runTI :: TI a -> a
runTI (TI f) = x
  where (_, _, x) = f nullSubst 0

getSubst :: TI Subst
getSubst = TI (\s n -> (s, n, s))

unify :: Type -> Type -> TI ()
unify t1 t2 = do s <- getSubst
                 u <- mgu (apply s t1) (apply s t2)
                 extSubst u

extSubst :: Subst -> TI ()
extSubst s' = TI (\s n -> (s' @@ s, n, ()))


newTVar :: Kind -> TI Type
newTVar k = TI (\s n -> let v = Tyvar (enumTID n) k
                        in (s, n+1, TVar v))

freshInst :: Scheme -> TI (Qual Type)
freshInst (Forall ks qt) = do ts <- mapM newTVar ks
                              return (inst ts qt)


class Instantiate t where
  inst :: [Type] -> t -> t

instance Instantiate Type where
  inst ts (TAp l r) = TAp (inst ts l) (inst ts r)
  inst ts (TGen n)  = ts !! n
  inst _  t            = t

instance Instantiate a => Instantiate [a] where
  inst ts = map (inst ts)

instance Instantiate t => Instantiate (Qual t) where
  inst ts (ps :=> t) = inst ts ps :=> inst ts t

instance Instantiate Pred where
  inst ts (IsIn c t) = IsIn c (inst ts t)

