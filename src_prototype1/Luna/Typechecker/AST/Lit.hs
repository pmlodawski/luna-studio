{-# LANGUAGE ScopedTypeVariables #-}

module Luna.Typechecker.AST.Lit (
    Lit(..),tiLit
  ) where


import Luna.Typechecker.TIMonad          (TI, newTVar)
import Luna.Typechecker.Typeclasses      (Pred(..))

import Luna.Typechecker.AST.ClassID      (ClassID(..))
import Luna.Typechecker.AST.Kind         (Kind(..))
import Luna.Typechecker.AST.Type         (Type(..), tChar, tInteger, tFloat, tString)

import Luna.Typechecker.AST.Internal.Lit (Lit(..))

import Luna.Typechecker.Internal.Logger


tiLit :: Lit -> TCLoggerT TI ([Pred], Type)
tiLit (LitChar _)     = return ([], tChar)
tiLit (LitInt _)      = return ([], tInteger)
tiLit (LitIntegral _) = do t <- newTVar Star
                           return ([IsIn (ClassID "Integral") t], t)
tiLit (LitFloat _)    = return ([], tFloat)
tiLit (LitStr _)      = return ([], tString)
