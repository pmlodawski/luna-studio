{-# LANGUAGE DeriveGeneric #-}

module Luna.Typechecker.Internal.Typeclasses (
    Pred(..),Qual(..),ClassEnv(..)
  ) where

import Luna.Typechecker.AST.Type       (Type(..))

import Luna.Typechecker.Substitutions  (Types(..))

import Luna.Typechecker.AST.TID        (TID)


import Data.List                       (intercalate,union,nubBy)
import Data.Function                   (on)
import Text.Printf                     (printf)
import Control.DeepSeq

import GHC.Generics





data Pred = IsIn TID Type
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



type Class = ([TID], [Inst])
type Inst  = Qual Pred


data ClassEnv = ClassEnv {
                  classes :: TID -> Maybe Class,
                  classesNames :: [(TID, Class)],
                  defaults :: [Type]
                }


instance Show ClassEnv where
  show (ClassEnv _ nm _) = printf "(classenv: %s)" (intercalate ", " $ map show $ nubBy ((==) `on` fst) nm)
  --show  = printf "(classenv: %s)" . intercalate ", " . map show . nubBy ((==) `on` fst) . classesNames

