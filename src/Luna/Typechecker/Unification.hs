module Luna.Typechecker.Unification (
    mgu
  ) where

import Luna.Typechecker.Substitution (Subst(..),Types(..))
import Luna.Typechecker.TIMonad      (TCLoggerT)
import Luna.Typechecker.Type         (Type(..),Tyvar)

import Logger (err,function,functionResult)

import Data.Monoid



mgu :: (Monad m) => Type -> Type -> TCLoggerT m Subst
mgu _ _ = error "mgu not implemented"
--mgu a b = functionResult "mgu" [show a, show b] $ aux a b
--  where
--    aux (TAp t1 t2) (TAp s1 s2)            = functionResult "mgu(aux)#1" (map show [TAp t1 t2, TAp s1 s2]) $ do
--                                               ts1 <- aux t1 s1
--                                               ts2 <- aux (apply ts1 t2) (apply ts1 s2)
--                                               return (ts1 `mappend` ts2)
--    aux (TConst t1) (TConst t2) | t1 == t2 = functionResult "mgu(aux)#2" (map show [TConst t1, TConst t2]) $ return mempty
--    aux (TConst t1) (TConst t2)            = functionResult "mgu(aux)#3" (map show [TConst t1, TConst t2]) $ err "cantunify" "Could not unify, consts were not equal"
--    aux (TVar u)   t                       = functionResult "mgu(aux)#4" (map show [TVar u, t])            $ varBind u t
--    aux t (TVar u)                         = functionResult "mgu(aux)#5" (map show [t, TVar u])            $ varBind u t
--    aux t1 t2                              = function "mgu(aux)#_" [show t1, show t2] $
--                                               err "cantunify" "either there's no implementation for that or it's a type error"

varBind :: (Monad m) => Tyvar -> Type -> TCLoggerT m Subst
varBind = error "varBind not implemented"
--varBind u t | t == TVar u        = functionResult "varBind#1" [show u, show t] $ return mempty
--            | u `elem` ftv t     = functionResult "varBind#2" [show u, show t] $ err "infinitetype" "blehbleh"
--            | otherwise          = functionResult "varBind#3" [show u, show t] $ return (Subst [(u, t)])