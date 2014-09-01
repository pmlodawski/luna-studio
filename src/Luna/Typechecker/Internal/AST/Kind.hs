module Luna.Typechecker.Internal.AST.Kind (Kind(..)) where

import           Text.Printf                                (printf)
import           Data.List                                  (intercalate)

data Kind = Star
          | Kfun Kind Kind
          deriving (Eq)

instance Show Kind where
  show Star = "*"
  show (Kfun x y) = show' x ++ "->" ++ show y
    where show' Star = show Star
          show' k = "(" ++ show k ++ ")"
  showList ks s = printf "%s[%s]" s (intercalate ", " (zipWith (\n k -> printf "gen{%d}::%s" (n::Int) $ show k) [0..] ks))