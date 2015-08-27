module Luna.Syntax.Name where

import Flowbox.Prelude
import Data.Maybe      (isJust)

type Name = String

class HasName a where
    name :: Lens' (a t) t

class                         MaybeNamed a where checkName :: a t -> Maybe t
                                                 default checkName :: HasName a => a t -> Maybe t
                                                 checkName = Just . view name
                                                 hasName   :: a t -> Bool
                                                 hasName   = isJust . checkName
instance {-# OVERLAPPABLE #-} MaybeNamed a where checkName = const Nothing