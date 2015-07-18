{-# LANGUAGE UndecidableInstances #-}

module Data.Repr where

import Prelude

class Repr a where
    repr :: a -> String

instance {-# OVERLAPPABLE #-} Show a => Repr a where
    repr = show