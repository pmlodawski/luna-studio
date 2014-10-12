{-# LANGUAGE DeriveGeneric #-}


module Luna.Typechecker.AST.Internal.Scheme (
    Scheme(..)
  ) where


import Luna.Typechecker.AST.Kind      (Kind(..))
import Luna.Typechecker.AST.Type      (Type(..),Dependent(..))

import Luna.Typechecker.Substitutions (Types(..))
import Luna.Typechecker.Typeclasses   (Qual(..))

import Control.DeepSeq

import Data.List                      (intercalate)

import GHC.Generics

import Text.Printf                    (printf)


data Scheme = Forall [Kind] (Qual Type)
            deriving (Eq,Generic)


instance Types Scheme where
  apply s (Forall ks qt) = Forall ks (apply s qt)
  tv (Forall _ qt) = tv qt

instance Show Scheme where
  show (Forall ks (ps :=> t)) = printf "∀%s ∃[%s] : %s => %s" (show ks) (listify (deps t)) (show ps) (show t)
    where listify = intercalate ", " . map (\(k,v) -> show k ++ "↦" ++ show v)

instance NFData Scheme
