{-# LANGUAGE DeriveGeneric, TupleSections #-}


module Luna.Typechecker.AST.Internal.Type (
    Type(..),Tyvar(..),Tycon(..),
    Dependent(..)
  ) where


import Luna.Typechecker.AST.Kind    (Kind(..))
import Luna.Typechecker.AST.TID     (TID(..))
import Luna.Typechecker.AST.VarID   (VarID(..))

import Control.DeepSeq

import Data.List                    (intercalate)

import qualified Data.Map.Strict as M

import GHC.Generics

import Text.Printf                  (printf)


class Dependent a where
  deps :: a -> [(Type,VarID)] -- ^ List of (a,b) where a ↦ b


data Type = TVar Tyvar
          | TCon Tycon
          | TAp Type Type
          | TGen Int
          | TStruct Type (M.Map VarID Type)
          deriving (Eq,Generic)


instance Dependent Type where
  deps (TStruct s m) = map (s,) (M.keys m) ++ concatMap recurse (M.elems m)
    where recurse (TStruct s' m') = deps (TStruct s' m')
          recurse _               = []
  deps _             = []


instance Show Type where
  show (TStruct s m) = printf "%s{%s}" (show s) (showmap ml)
    where ml = M.toList m
          showmap :: [(VarID,Type)] -> String
          showmap = intercalate "," . map (\(k,a) -> show k ++ "::" ++ show a)
  show (TVar (Tyvar n k)) = printf "(%s::%s)" (show n) (show k)
  show (TCon (Tycon n k)) = printf "(%s::%s)" (show n) (show k)
  show (TAp (TAp x t1) t2)
    | x == TCon (Tycon (TID "(->)") (Kfun Star (Kfun Star Star)))
                          = printf "%s -> %s" (show t1) (show t2)
  show (TAp t1 t2)        = printf "(%s %s)" (show t1) (show t2)
  show (TGen i)           = printf "gen{%d}" i
  showList cs s = s ++ unwords (map show cs)

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
