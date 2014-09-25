{-# LANGUAGE DeriveGeneric #-}


module Luna.Typechecker.AST.Internal.Scheme (
    Scheme(..)
  ) where


import Luna.Typechecker.AST.Kind      (Kind(..))
import Luna.Typechecker.AST.Type      (Type(..))

import Luna.Typechecker.Substitutions (Types(..))
import Luna.Typechecker.Typeclasses   (Qual(..))


import Control.DeepSeq
import GHC.Generics

import Text.Printf                    (printf)


data Scheme = Forall [Kind] (Qual Type)
            deriving (Eq,Generic)


instance Types Scheme where
  apply s (Forall ks qt) = Forall ks (apply s qt)
  tv (Forall _ qt) = tv qt

instance Show Scheme where
  show (Forall [] ([] :=> t)) = printf "%s" (show t)
  show (Forall [] (ps :=> t)) = printf "%s => %s" (show ps) (show t)
  show (Forall ks ([] :=> t)) = printf "forall %s. %s" (show ks) (show t)
  show (Forall ks (ps :=> t)) = printf "forall %s. %s => %s" (show ks) (show ps) (show t)

instance NFData Scheme
