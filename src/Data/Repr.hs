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


instance {-# OVERLAPPABLE #-} Repr a => Repr (Maybe a) where
    repr (Just a) = "Just (" <> repr a <> ")"
    repr Nothing  = "Nothing"

instance Repr Char where repr = show
instance Repr Int  where repr = show

instance (Repr t1, Repr t2) => Repr (t1,t2) where repr (t1, t2) = "(" <> repr t1 <> ", " <> repr t2