module Luna.Typechecker.Data (
    TVar, Var,
    Fieldlabel, Field,
    Subst, Typo,
    Type(..), Predicate(..), Constraint(..), TypeScheme(..)
  ) where


type TVar = Int

type Var = Int

type Fieldlabel = Var

type Field = (Fieldlabel, Type)

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

type Subst = [(TVar, Type)]

type Typo = [(Var,TypeScheme)]
