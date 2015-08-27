{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE GADTs                  #-}

module Data.Content where

import Prelude
import Control.Lens

type Content' a c = Content a a c c

class Content a a' c c' | a -> c, a' -> c' where
    content :: Lens a a' c c'

instance {-# OVERLAPPABLE #-} (a ~ c, a' ~ c') => Content a a' c c' where content = lens id (flip const)