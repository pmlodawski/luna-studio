module Luna.Typechecker.Type.Type (
    Type(..), Tyvar(..), Tycon(..)
  ) where


import Luna.Typechecker.AST.IDs (TyID)


data Type = TVar Tyvar
          | TConst Tycon
          | TAp Type Type
          deriving (Eq)

data Tyvar = Tyvar TyID
           deriving (Eq)

data Tycon = Tycon TyID
           deriving (Eq)