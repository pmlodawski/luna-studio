{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Repr where

import Prelude

class Repr a where
    repr :: a -> String

instance Show a => Repr a where
    repr = show