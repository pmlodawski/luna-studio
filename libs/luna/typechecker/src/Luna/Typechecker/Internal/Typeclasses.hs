{-# LANGUAGE DeriveGeneric #-}


module Luna.Typechecker.Internal.Typeclasses (
    Pred(..),Qual(..),ClassEnv(..)
  ) where


import           Luna.Typechecker.Substitutions  (Types(..))

import           Luna.Typechecker.AST.ClassID    (ClassID)
import           Luna.Typechecker.AST.Type       (Type(..))

import           Control.DeepSeq

import           Data.List                       (intercalate,union)

import qualified Data.Map.Strict                 as M

import           GHC.Generics

import           Text.Printf                     (printf)


data Pred = IsIn ClassID Type
          deriving (Eq,Generic)


instance Types Pred where
  apply s (IsIn i t) = IsIn i (apply s t)
  tv (IsIn _ t)      = tv t

instance NFData Pred

instance Show Pred where
  show (IsIn tid ty) = printf "%s %s" (show ty) tid


data Qual t = [Pred] :=> t
            deriving (Eq,Generic)


instance Types t => Types (Qual t) where
  apply s (ps :=> t) = apply s ps :=> apply s t
  tv (ps :=> t)      = tv ps `union` tv t

instance NFData t => NFData (Qual t)

instance Show t => Show (Qual t) where
  show (ps :=> t) = printf "(%s :=> %s)" (show ps) (show t)


type Class = ([ClassID], [Inst]) -- ^ super-classes, instances
type Inst  = Qual Pred

data ClassEnv = ClassEnv {
                  classes :: M.Map ClassID Class,
                  defaults :: [Type]
                }

instance Show ClassEnv where
  show ce = printf "(classenv: %s)" (intercalate ", " . map show . M.toList . classes $ ce)
