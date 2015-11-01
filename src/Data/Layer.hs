module Data.Layer where

import Control.Lens
import Prelude


type family Unlayered l

class Layered l where
    layered :: Lens' l (Unlayered l)
    default layered :: (Unlayered l ~ Unwrapped l, Wrapped l) => Lens' l (Unlayered l)
    layered = _Wrapped'

class Layered l => Layer l where
    layer :: Unlayered l -> l

unlayer :: Layered l => l -> Unlayered l
unlayer = view layered