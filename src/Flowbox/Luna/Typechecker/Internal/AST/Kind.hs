module Flowbox.Luna.Typechecker.Internal.AST.Kind (
    Kind(..),
    HasKind
  )where

-- | Kind of a type.
-- Example values: Star, (Star `Kfun` Star) `Kfun` Star, Star `Kfun` (Star `Kfun` Star)
data Kind = Star
          | Kfun Kind Kind
          deriving (Eq)

instance Show Kind where
  show Star = "*"
  show (Kfun x y) = show' x ++ " -> " ++ show' y
    where show' Star = show Star
          show' x = "(" ++ show x ++ ")"


class HasKind t where
    kind :: t -> Kind -- ^ Determine the kind of a type variable, type constant, or type expression.

instance HasKind Tyvar where
  kind (Tyvar _ k) = k

instance HasKind Tycon where
  kind (Tycon _ k) = k

instance HasKind Type where
  kind (TVar u)   = kind u
  kind (TCon tc)  = kind tc
  kind (TAp t t') = case (kind t) of
                         (Kfun k' k) -> k
                         _           -> error "kind mismatch"
    where k' = kind t'

-- TODO [kgdk] 14 sie 2014: zmienić typ zwracany kind na Either by obsługiwać errory
