module Luna.Typechecker.Data (
    TVar, Var,
    Fieldlabel, Field,
    Subst, Typo,
    Type(..), Predicate(..), Constraint(..), TypeScheme(..),
    TypeMap,
    true_cons, null_subst, init_typo
  ) where


import Data.Map.Strict  (Map)

import Luna.Syntax.Enum (ID)



type TVar       = Int
type Var        = Int
type Fieldlabel = Var
type Field      = (Fieldlabel, Type)
type TypeMap    = Map ID Type
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
empty_typo = []     -- TODO [kgdk] 22 sty 2015: make a monoid


true_cons :: Constraint
true_cons = C [TRUE]


null_subst :: Subst
null_subst = []


init_typo :: [Typo]
init_typo = [empty_typo]