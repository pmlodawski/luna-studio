module Flowbox.Luna.Typechecker.Internal.TIMonad () where

import qualified Flowbox.Luna.Typechecker.Internal.AST.AST    as AST
import qualified Flowbox.Luna.Typechecker.Internal.AST.Common as Com
import qualified Flowbox.Luna.Typechecker.Internal.AST.Expr   as Exp
import qualified Flowbox.Luna.Typechecker.Internal.AST.Kind   as Knd
import qualified Flowbox.Luna.Typechecker.Internal.AST.Lit    as Lit
import qualified Flowbox.Luna.Typechecker.Internal.AST.Module as Mod
import qualified Flowbox.Luna.Typechecker.Internal.AST.Pat    as Pat
import qualified Flowbox.Luna.Typechecker.Internal.AST.TID    as TID
import qualified Flowbox.Luna.Typechecker.Internal.AST.Type   as Ty

import           Flowbox.Luna.Data.AST.Common                 (ID(..))
import           Flowbox.Luna.Typechecker.Internal.AST.TID    (TID(..))



-- TODO [kgdk] 19 sie 2014: a może użyć State/ST zamiast newtype TI a?
-- TODO [kgdk] 19 sie 2014: zbadać performance 'State (x,y)' vs to tutaj vs 'StateT x (State y)'
newtype TI a = TI (Subst -> Int -> (Subst, Int, a))

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
  where (s, n, x) = f nullSubst 0

getSubst :: TI Subst
getSubst = TI (\s n -> (s, n, s))

unify :: Ty.Type -> Ty.Type -> TI ()
unify t1 t2 = do s <- getSubst
                 u <- mgu (apply s t1) (apply s t2)
                 extSubst u

extSubst :: Subst -> TI ()
extSubst s' = TI (\s n -> (s' @@ s, n, ()))


newTVar :: Knd.Kind -> TI Ty.Type
newTVar k = TI (\s n -> let v = Ty.Tyvar (enumTID n) k
                        in (s, n+1, Ty.TVar v))

freshInst :: Scheme -> TI (Qual Ty.Type)
freshInst (Forall ks qt) = do ts <- mapM newTVar ks
                              return (inst ts qt)



class Instantiate t where
  inst :: [Ty.Type] -> t -> t

instance Instantiate Ty.Type where
  inst ts (Ty.TAp l r) = Ty.TAp (inst ts l) (inst ts r)
  inst ts (Ty.TGen n)  = ts !! n
  inst ts t            = t

instance Instantiate a => Instantiate [a] where
  inst ts = map (inst ts)

instance Instantiate t => Instantiate (Qual t) where
  inst ts (ps :=> t) = inst ts ps :=> inst ts t

instance Instantiate Pred where
  inst ts (IsIn c t) = IsIn c (inst ts t)

