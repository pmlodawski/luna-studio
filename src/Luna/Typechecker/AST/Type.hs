{-# LANGUAGE DeriveGeneric, TupleSections #-}

module Luna.Typechecker.AST.Type (
  ) where


import Luna.Typechecker.AST.IDs (TyID)

import GHC.Generics


data Type = TVar Tyvar
          | TConst Tycon
          | TAp Type Type
          deriving (Eq,Generic)

data Tyvar = Tyvar TyID -- Kind
           deriving (Eq,Generic)

data Tycon = Tycon TyID -- Kind
           deriving (Eq,Generic)


