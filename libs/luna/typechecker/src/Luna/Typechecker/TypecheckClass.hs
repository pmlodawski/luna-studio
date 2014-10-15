module Luna.Typechecker.TypecheckClass (
    TCLoggerT, Inference(..)
  ) where

import Luna.Typechecker.AST
import Luna.Typechecker.Substitution
import Luna.Typechecker.Type
import Luna.Typechecker.TypeEnv
import Luna.Typechecker.TIMonad
import Luna.Typechecker.Unification

import Luna.Typechecker.RefactorMePlease -- don't say nothing

import Data.List
import Data.Monoid



generalise :: TypeEnv -> Type -> TILogger Scheme
generalise env t = return $ Scheme vars t
  where vars = map (\(Tyvar x) -> x) (ftv t \\ ftv env)


class Inference a where
  infer :: TypeEnv -> a -> TILogger (Subst, Type)

instance Inference Lit where
  infer _ (LitChar _)   = return (mempty, tChar)
  infer _ (LitDouble _) = return (mempty, tDouble)
  infer _ (LitInt _)    = return (mempty, tInt)
  infer _ (LitStr _)    = return (mempty, tString)

instance Inference Expr where
  infer env (EVar var)  = case getTypeEnv env var of
                             Nothing  -> err "notfounderr" $ "sorry, but " ++ show var ++ " could not be found in env: " ++ show env
                             Just sch -> do ty <- instantiate sch
                                            return (mempty, ty)
  infer env (ELit lit)           = infer env lit
  infer env (EApp f a)           = do tv <- mkTyID
                                      (sf,tf) <- infer env f
                                      (sa,ta) <- infer (apply sf env) a
                                      s3 <- mgu (apply sa tf) (ta `mkTyFun` tv)
                                      return (s3 `mappend` sa `mappend` sf, apply s3 tv)
  infer env (EAbs vid body)      = do tv <- mkTyID
                                      let env' = expandTypeEnv env vid (Scheme [] tv)
                                      (s1,t1) <- infer env' body
                                      return (s1, mkTyFun (apply s1 tv) t1)
  infer env (ELet name val body) = do (valSubst, valType) <- infer env val
                                      valTypeGen          <- generalise (apply valSubst env) valType
                                      let env' = expandTypeEnv env name valTypeGen
                                      (bodSubst, bodType) <- infer (apply valSubst env') body
                                      return (valSubst `mappend` bodSubst, bodType)