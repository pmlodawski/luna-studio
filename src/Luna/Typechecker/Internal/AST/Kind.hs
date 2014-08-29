module Luna.Typechecker.Internal.AST.Kind (Kind(..)) where




-- | Kind of a type.
-- Example values: Star, (Star `Kfun` Star) `Kfun` Star, Star `Kfun` (Star `Kfun` Star)
data Kind = Star
          | Kfun Kind Kind
          deriving (Eq)

instance Show Kind where
  show Star = "*"
  show (Kfun x y) = show' x ++ "->" ++ show y
    where show' Star = show Star
          show' k = "(" ++ show k ++ ")"
