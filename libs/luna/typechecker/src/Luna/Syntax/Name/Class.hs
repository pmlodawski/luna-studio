{-# LANGUAGE UndecidableInstances #-}

module Luna.Syntax.Name.Class where

import Prelude.Luna
import FastString


-- === Definitions === --

type family Name a
class    HasName a where name :: Lens' a (Name a)


type NameBase = FastString


-- === Instances === --

-- Construction
type instance Deconstructed NameBase = String
instance Maker   NameBase where make  = fsLit    ; {-# INLINE make  #-}
instance Breaker NameBase where break = unpackFS ; {-# INLINE break #-}

-- Repr
instance Repr s String => Repr s NameBase where repr = repr ∘ break
