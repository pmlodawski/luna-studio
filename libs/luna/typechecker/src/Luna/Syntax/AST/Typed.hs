{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}

module Luna.Syntax.AST.Typed where

import Data.Construction
import Prologue

-- === Typed ===

data Typed t a = Typed t a deriving (Show, Functor, Traversable, Foldable)


class HasType a t | a -> t where tp :: Lens' a t
instance {-# OVERLAPPABLE #-} (HasType (Unlayered a) t, Layered a)
                           => HasType a           t where tp = layered . tp
instance {-# OVERLAPPABLE #-} HasType (Typed t a) t where tp = lens (\(Typed t _) -> t) (\(Typed _ a) t -> Typed t a)


-- === Typed layer ===

type instance Unlayered  (Typed t a) = a
type instance Destructed (Typed t a) = a
instance      Layered    (Typed t a) where layered = lens (\(Typed _ a) -> a) (\(Typed t _) a -> Typed t a)

--class Destructor  m a where destruct  :: a -> m (Destructed a)
