{-# LANGUAGE UndecidableInstances #-}

module Data.Repr where

import Prelude
import Data.List (intercalate)
import Data.Monoid

class Repr a where
    repr :: a -> String

instance {-# OVERLAPPABLE #-} Show a => Repr a where
    repr = show

instance {-# OVERLAPPABLE #-} Repr a => Repr [a] where
    repr lst = "[" <> intercalate ", " (fmap repr lst) <> "]"