module Luna.Typechecker.TIMonad (
    TI(..),newTVar,freshInst,unify,getSubst,startTI,Instantiate(..)
  ) where


import Luna.Typechecker.Substitutions   (Types(..),Subst,nullSubst,(@@))
import Luna.Typechecker.Typeclasses     (Qual(..),Pred(..))
import Luna.Typechecker.Unification     (mgu)

import Luna.Typechecker.AST.Kind        (Kind)
import Luna.Typechecker.AST.Scheme      (Scheme(..))
import Luna.Typechecker.AST.TID         (enumTID)
import Luna.Typechecker.AST.Type        (Type(..), Tyvar(..))

import Luna.Typechecker.Internal.Logger

import Control.Applicative              (Applicative(..))
import Control.Monad                    (ap)

import Control.Monad.Trans              (lift)


newtype TI a = TI { runTI :: Subst -> Int -> (Subst, Int, a) }


instance Functor TI where
  fmap f (TI sia) = TI $ \s i -> let (s', i', a') = sia s i in (s', i', f a')

instance Applicative TI where
  pure = return
  (<*>) = ap

instance Monad TI where
  return x   = TI (\s n -> (s,n,x))
  TI f >>= g = TI (\s n -> case f s n of
                             (s',n',x) -> let TI gx = g x
                                           in gx s' n')


startTI :: TI a -> a
startTI (TI f) = x
  where (_, _, x) = f nullSubst 0

getSubst :: TCLoggerT TI Subst
getSubst = lift (TI (\s n -> (s, n, s)))

unify :: Type -> Type -> TCLoggerT TI ()
unify t1 t2 = do s <- getSubst
                 u <- mgu (apply s t1) (apply s t2)
                 extSubst u

extSubst :: Subst -> TCLoggerT TI ()
extSubst s' = lift (TI (\s n -> (s' @@ s, n, ())))


newTVar :: Kind -> TCLoggerT TI Type
newTVar k = lift $ TI (\s n -> let v = Tyvar (enumTID n) k
                                in (s, n+1, TVar v))

freshInst :: Scheme -> TCLoggerT TI (Qual Type)
freshInst (Forall ks qt) = do ts <- mapM newTVar ks
                              return (inst ts qt)


class Instantiate t where
  inst :: [Type] -> t -> t


instance Instantiate Type where
  -- TODO
  inst ts (TAp l r) = TAp (inst ts l) (inst ts r)
  inst ts (TGen n)  = ts !! n
  inst _  t         = t

instance Instantiate a => Instantiate [a] where
  inst ts = map (inst ts)

instance Instantiate t => Instantiate (Qual t) where
  inst ts (ps :=> t) = inst ts ps :=> inst ts t

instance Instantiate Pred where
  inst ts (IsIn c t) = IsIn c (inst ts t)

