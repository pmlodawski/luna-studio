module Luna.Typechecker.Type.Type (
    Type(..), Tyvar(..), Tycon(..)
  ) where


import Luna.Typechecker.IDs (TyID)


type Label = String

data Type = TVar Tyvar
          | TConst Tycon
          | TAp Type Type
          | TRow Tyrow
          deriving (Eq, Show)

data Tyvar = Tyvar { id :: VarID, constraint :: Constraint }
           deriving (Eq, Show)

-- data Tyvar = Tyvar { id :: VarID, kind :: Kind, constraint :: Constraint }
--            deriving (Eq, Show)

data Tycon = Tycon { id :: TyID, constraint :: Constraint }
           deriving (Eq, Show)

-- data Tycon = Tycon { id :: TyID, kind :: Kind, constraint :: Constraint }
--           deriving (Eq, Show)

-- | Row type variables may have constraints.
-- data Kind = Star | Row deriving (Eq)
