{-# LANGUAGE DeriveGeneric #-}


module Luna.Typechecker.AST.Internal.Kind (
    Kind(..)
  ) where


import Control.DeepSeq
import Data.List       (intercalate)
import GHC.Generics
import Text.Printf     (printf)


data Kind = Star
          | Kfun Kind Kind
          deriving (Eq,Generic)


instance NFData Kind

instance Show Kind where
  show Star = "*"
  show (Kfun x y) = showNopars x ++ "->" ++ show y
    where showNopars Star = show Star
          showNopars k = "(" ++ show k ++ ")"
  showList ks s = printf "%s[%s]" s (intercalate ", " (zipWith (\n k -> printf "gen{%d}::%s" (n::Int) $ show k) [0..] ks))
