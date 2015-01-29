module Luna.Typechecker.Data (
    TVar, Var,
    Fieldlabel, Field,
    Subst, Typo,
    Type(..), Predicate(..), Constraint(..), TypeScheme(..),
    TypeMap,
    null_subst, init_typo, true_cons
  ) where


import Data.Map.Strict  (Map)
import Data.Monoid

import Luna.Syntax.Enum (ID)


type TVar       = Int
type Var        = Int
type Fieldlabel = Var
type Field      = (Fieldlabel, Type)
type TypeMap    = Map ID (Typo, Type, Constraint)
type Subst      = [(TVar, Type)]
type Typo       = [(Var,TypeScheme)]


data Type = TV TVar
          | Type `Fun` Type
          | Record [Field]
          deriving (Show,Eq)


data Predicate  = TRUE
                | Type `Subsume` Type
                | Reckind Type Fieldlabel Type
                deriving (Show,Eq)


data Constraint = C [Predicate]
                | Proj [TVar] [Predicate]
                deriving (Show)


data TypeScheme = Mono Type
                | Poly [TVar] Constraint Type
                deriving (Show)


empty_typo :: Typo
empty_typo = []


null_subst :: Subst
null_subst = []


init_typo :: [Typo]
init_typo = [empty_typo]


true_cons :: Constraint
true_cons = C [TRUE]


instance Monoid Constraint where
  mempty = C [TRUE]
  mappend (C p1) (C p2)               = C (p1 ++ p2)
  mappend (C p1) (Proj tvr p2)        = Proj tvr (p1 ++ p2)
  mappend (Proj tvr p1) (C p2)        = Proj tvr (p1 ++ p2)
  mappend (Proj tv1 p1) (Proj tv2 p2) = Proj (tv1 ++ tv2) (p1 ++ p2)
