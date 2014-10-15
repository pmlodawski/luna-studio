{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Luna.Typechecker.TypecheckClass (
    TCLoggerT, Inference(..)
  ) where

import Luna.Typechecker.AST
import Luna.Typechecker.Substitution
import Luna.Typechecker.Type
import Luna.Typechecker.TypeEnv
import Luna.Typechecker.TIMonad

import Data.Monoid


generalise = undefined

class Inference a where
  infer :: (Monad m) => TypeEnv -> a -> TCLoggerT m (Subst, Type)

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
  infer env (ELit lit)   = infer env lit
  infer _   (EApp _ _)   = err "a3" "b3"
  infer _   (EAbs _ _)   = err "a4" "b4"
  infer env (ELet name val body) = do (valSubst, valType) <- infer env val
                                      valTypeGen          <- generalise (apply valSubst env) valType
                                      let env' = expandTypeEnv env name valTypeGen
                                      (bodSubst, bodType) <- infer (apply valSubst env') body
                                      return (valSubst `mappend` bodSubst, bodType)