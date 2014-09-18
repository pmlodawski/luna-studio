{-# LANGUAGE ScopedTypeVariables #-}

module Luna.Typechecker.AST.Lit (
    Lit(..),tiLit
  ) where

import Luna.Typechecker.AST.Internal.Lit (Lit(..))

import Luna.Typechecker.AST.Kind         (Kind(..))
import Luna.Typechecker.AST.Type         (Type(..), tChar, tInteger, tFloat, tString)

import Luna.Typechecker.TIMonad          (TI, newTVar)
import Luna.Typechecker.Typeclasses      (Pred(..))



tiLit :: Lit -> TI ([Pred], Type)
tiLit (LitChar _)     = return ([], tChar)
tiLit (LitInt _)      = return ([], tInteger)
tiLit (LitIntegral _) = do t <- newTVar Star
                           return ([IsIn "Integral" t], t)
tiLit (LitFloat _)    = return ([], tFloat)
tiLit (LitStr _)      = return ([], tString)