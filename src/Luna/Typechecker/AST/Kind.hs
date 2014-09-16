module Luna.Typechecker.AST.Kind (Kind(..)) where

import Text.Printf (printf)
import Data.List   (intercalate)

import Control.DeepSeq

data Kind = Star
          | Kfun Kind Kind
          deriving (Eq)

instance NFData Kind where
  rnf Star = ()
  rnf (Kfun k1 k2) = rnf k1 `seq` rnf k2

instance Show Kind where
  show Star = "*"
  show (Kfun x y) = show' x ++ "->" ++ show y
    where show' Star = show Star
          show' k = "(" ++ show k ++ ")"
  showList ks s = printf "%s[%s]" s (intercalate ", " (zipWith (\n k -> printf "gen{%d}::%s" (n::Int) $ show k) [0..] ks))
