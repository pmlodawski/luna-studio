{-# LANGUAGE ScopedTypeVariables #-}

module Luna.Typechecker.AST.Lit (Lit(..),tiLit) where

import Luna.Typechecker.AST.Kind         (Kind(..))
import Luna.Typechecker.AST.Type         (Type(..), tChar, tInteger, tFloat, tString)

import Luna.Typechecker.TIMonad          (TI, newTVar)
import Luna.Typechecker.Typeclasses      (Pred(..))

import Text.Printf                                (printf)

data Lit = LitChar     Char
         | LitFloat    Float
         | LitInt      Integer
         | LitIntegral Int
         | LitStr      String


tiLit :: Lit -> TI ([Pred], Type)
tiLit (LitChar _)     = return ([], tChar)
tiLit (LitInt _)      = return ([], tInteger)
tiLit (LitIntegral _) = do t <- newTVar Star
                           return ([IsIn "Integral" t], t)
tiLit (LitFloat _)    = return ([], tFloat)
tiLit (LitStr _)      = return ([], tString)


instance Show Lit where
  show (LitChar    c)  = printf "'%c'" c
  show (LitFloat   f)  = printf "%.3f" f
  show (LitInt     i)  = show i
  show (LitIntegral i) = show i
  show (LitStr     s)  = show s
