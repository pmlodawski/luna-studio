module Flowbox.Luna.Typechecker.Internal.TIMonad (TI, newTVar, freshInst, unify, getSubst, runTI) where

import qualified Flowbox.Luna.Typechecker.Internal.AST.Kind         as Knd
import qualified Flowbox.Luna.Typechecker.Internal.AST.Scheme       as Sch
import qualified Flowbox.Luna.Typechecker.Internal.AST.TID          as TID
import qualified Flowbox.Luna.Typechecker.Internal.AST.Type         as Ty

import qualified Flowbox.Luna.Typechecker.Internal.Substitutions    as Sub
import qualified Flowbox.Luna.Typechecker.Internal.Typeclasses      as Tcl
import qualified Flowbox.Luna.Typechecker.Internal.Unification      as Unf




-- TODO [kgdk] 19 sie 2014: a może użyć State/ST zamiast newtype TI a?
-- TODO [kgdk] 19 sie 2014: zbadać performance 'State (x,y)' vs to tutaj vs 'StateT x (State y)'
-- TODO [kgdk] 21 sie 2014: zrobic z tego Applicative
newtype TI a = TI (Sub.Subst -> Int -> (Sub.Subst, Int, a))


instance Monad TI where
  return x   = TI (\s n -> (s,n,x))
  TI f >>= g = TI (\s n -> case f s n of
                             (s',n',x) -> let TI gx = g x
                                           in gx s' n')



-- TODO [kgdk] 19 sie 2014: co to jest z tymi nazwami? :< to powinno być nazwą pola rekordu natomiast
-- to co tutaj to powinno się nazywać… runTIWithDefault lub podobnie oraz getTI
-- TODO [kg] 15 lip 2014: opcjonalnie zrobić instance MonadState TI
runTI :: TI a -> a
runTI (TI f) = x
  where (s, n, x) = f Sub.nullSubst 0

getSubst :: TI Sub.Subst
getSubst = TI (\s n -> (s, n, s))

unify :: Ty.Type -> Ty.Type -> TI ()
unify t1 t2 = do s <- getSubst
                 u <- Unf.mgu (Sub.apply s t1) (Sub.apply s t2)
                 extSubst u

extSubst :: Sub.Subst -> TI ()
extSubst s' = TI (\s n -> (s' Sub.@@ s, n, ()))


newTVar :: Knd.Kind -> TI Ty.Type
newTVar k = TI (\s n -> let v = Ty.Tyvar (TID.enumTID n) k
                        in (s, n+1, Ty.TVar v))

freshInst :: Sch.Scheme -> TI (Tcl.Qual Ty.Type)
freshInst (Sch.Forall ks qt) = do ts <- mapM newTVar ks
                                  return (inst ts qt)


class Instantiate t where
  inst :: [Ty.Type] -> t -> t

instance Instantiate Ty.Type where
  inst ts (Ty.TAp l r) = Ty.TAp (inst ts l) (inst ts r)
  inst ts (Ty.TGen n)  = ts !! n
  inst ts t            = t

instance Instantiate a => Instantiate [a] where
  inst ts = map (inst ts)

instance Instantiate t => Instantiate (Tcl.Qual t) where
  inst ts (ps Tcl.:=> t) = inst ts ps Tcl.:=> inst ts t

instance Instantiate Tcl.Pred where
  inst ts (Tcl.IsIn c t) = Tcl.IsIn c (inst ts t)

