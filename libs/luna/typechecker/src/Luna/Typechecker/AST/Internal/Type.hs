{-# LANGUAGE DeriveGeneric #-}


module Luna.Typechecker.AST.Internal.Type (
    Type(..),Tyvar(..),Tycon(..)
  ) where


import Luna.Typechecker.AST.Kind (Kind(..))
import Luna.Typechecker.AST.TID  (TID)

import Control.DeepSeq

import GHC.Generics

import Text.Printf               (printf)


data Type = TVar Tyvar
          | TCon Tycon
          | TAp Type Type
          | TGen Int
          deriving (Eq,Generic)


instance Show Type where
  show (TVar (Tyvar n k)) = printf "(%s::%s)" n (show k)
  show (TCon (Tycon n k)) = printf "(%s::%s)" (show n) (show k)
  show (TAp (TAp x t1) t2)
    | x == TCon (Tycon "(->)" (Kfun Star (Kfun Star Star)))
                          = printf "%s -> %s" (show t1) (show t2)
  show (TAp t1 t2)        = printf "(%s %s)" (show t1) (show t2)
  show (TGen i)           = printf "gen{%d}" i
  showList cs s = printf "%s%s" s (unwords $ map show cs)

instance NFData Type


data Tyvar = Tyvar TID Kind
           deriving (Eq,Generic)


instance Show Tyvar where
  show x = show (TVar x)

instance NFData Tyvar


data Tycon = Tycon TID Kind
           deriving (Eq,Generic)


instance Show Tycon where
  show x = show (TCon x)

instance NFData Tycon
