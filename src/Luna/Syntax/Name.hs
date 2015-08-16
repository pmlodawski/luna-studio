module Luna.Syntax.Name where

import Flowbox.Prelude


type Name = String

class HasName a where
    name :: Lens' a Name