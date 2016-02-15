module Data.Direction where

import Control.Lens


type family Source a
type family Target a

class HasSource a where source :: Lens' a (Source a)
class HasTarget a where target :: Lens' a (Target a)
