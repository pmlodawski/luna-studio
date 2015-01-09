module Luna.Typechecker.Data (
    TVar, Var,
    Fieldlabel, Field,
    Subst, Typo,
    Type(..), Predicate(..), Constraint(..), TypeScheme(..),
    true_cons, null_subst, init_typo
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


true_cons :: Constraint
true_cons = C [TRUE]

null_subst :: Subst
null_subst = []

init_typo :: Typo
init_typo = []