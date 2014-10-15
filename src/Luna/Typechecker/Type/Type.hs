module Luna.Typechecker.Type.Type (
    Type(..), Tyvar(..), Tycon(..)
  ) where


import Luna.Typechecker.IDs (TyID)


data Type = TVar Tyvar
          | TConst Tycon
          | TAp Type Type
          deriving (Eq, Show)

data Tyvar = Tyvar TyID
           deriving (Eq, Show)

data Tycon = Tycon TyID
           deriving (Eq, Show)
