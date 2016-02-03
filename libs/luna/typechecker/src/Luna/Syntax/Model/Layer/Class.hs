{-# LANGUAGE UndecidableInstances #-}

module Luna.Syntax.Model.Layer.Class where

import Prologue

import Data.Attribute
import Data.Construction
import Data.Layer.Cover
import Type.Bool
import Luna.Syntax.AST.Term (LayoutType)


--------------------
-- === Layers === --
--------------------

-- === Definitions === --

newtype Layer layout t base = Layer (LayerData layout t base) -- deriving (Show) --, Eq, Ord, Functor, Traversable, Foldable)


--newtype Layer layout t a = Layer a      deriving (Show, Eq, Ord, Functor, Traversable, Foldable)
data    Attached     t a = Attached t a deriving (Show, Eq, Ord, Functor, Traversable, Foldable)
type family LayerData t d a


-- === Instances === --

deriving instance Show (Unwrapped (Layer l t a)) => Show (Layer l t a)

-- Wrappers

makeWrapped ''Layer
type instance Unlayered (Layer l t a) = Unwrapped (Layer l t a)
instance      Layered   (Layer l t a)


--makeWrapped ''Layer
--type instance Unlayered (Layer l t a) = a
--instance      Layered   (Layer l t a)

type instance Unlayered (Attached t a) = a
instance      Layered   (Attached t a) where
    layered = lens (\(Attached _ a) -> a) (\(Attached d _) a -> Attached d a) ; {-# INLINE layered #-}

-- Construction

instance (Maker m t, Functor m) => LayerConstructor m (Attached t a) where
    constructLayer a = flip Attached a <$> make ; {-# INLINE constructLayer #-}

-- Casting

instance (Castable a a', Castable t t') => Castable (Attached t a) (Attached t' a') where
    cast (Attached d a) = Attached (cast d) (cast a) ; {-# INLINE cast #-}

instance Castable (Unwrapped (Layer l t a)) (Unwrapped (Layer l' t' a')) => Castable (Layer l t a) (Layer l' t' a') where
    cast = wrapped %~ cast ; {-# INLINE cast #-}

--instance Castable a a' => Castable (Layer l t a) (Layer l' t a') where
--    cast = wrapped %~ cast ; {-# INLINE cast #-}

-- Attributes

--type instance Attr a (Attached (Layer l t u) base) = If (a == t) u (Attr a base)
--instance {-# OVERLAPPABLE #-} (HasAttr     a base, Attr a (Attached (Layer l t u) base) ~ Attr a base) => HasAttr     a (Attached (Layer l t u) base) where attr      = layered ∘∘  attr                                                           ; {-# INLINE attr      #-}
--instance {-# OVERLAPPABLE #-}                                                                             HasAttr     a (Attached (Layer l a u) base) where attr _    = lens (\(Attached d _) -> d) (\(Attached _ a) d -> Attached d a) ∘ wrapped' ; {-# INLINE attr      #-}
--instance {-# OVERLAPPABLE #-} (MayHaveAttr a base, Attr a (Attached (Layer l t u) base) ~ Attr a base) => MayHaveAttr a (Attached (Layer l t u) base) where checkAttr = layered ∘∘∘ checkAttr                                                      ; {-# INLINE checkAttr #-}
--instance {-# OVERLAPPABLE #-}                                                                             MayHaveAttr a (Attached (Layer l a u) base)


type instance Attr a (Attached (Layer l t u) base) = If (a == t) (Unwrapped (Layer l t u)) 
                                                                 (Attr a base)
instance {-# OVERLAPPABLE #-} (HasAttr     a base, Attr a (Attached (Layer l t u) base) ~ Attr a base) => HasAttr     a (Attached (Layer l t u) base) where attr      = layered ∘∘  attr                                                           ; {-# INLINE attr      #-}
instance {-# OVERLAPPABLE #-}                                                                              HasAttr     a (Attached (Layer l a u) base) where attr _    = lens (\(Attached d _) -> d) (\(Attached _ a) d -> Attached d a) ∘ wrapped' ; {-# INLINE attr      #-}
instance {-# OVERLAPPABLE #-} (MayHaveAttr a base, Attr a (Attached (Layer l t u) base) ~ Attr a base) => MayHaveAttr a (Attached (Layer l t u) base) where checkAttr = layered ∘∘∘ checkAttr                                                      ; {-# INLINE checkAttr #-}
instance {-# OVERLAPPABLE #-}                                                                              MayHaveAttr a (Attached (Layer l a u) base)



--------------------
-- === Shell === ---
--------------------

data (layers :: [*]) :< (a :: [*] -> *) = Shell (ShellStrcture layers (a layers))

type family ShellStrcture ls a where 
    ShellStrcture '[]       a = Cover a
    ShellStrcture (l ': ls) a = AttachedLayer l (ShellStrcture ls a)

type AttachedLayer t a = Attached (Layer (LayoutType (Uncovered a)) t (Uncovered a)) a


-- === Utils === ---

type family Shelled a where Shelled (t ls) = ls :< t


-- === Instances === --

deriving instance Show (Unwrapped (ls :< a)) => Show (ls :< a)

makeWrapped ''(:<)
type instance Unlayered (ls :< a) = Unwrapped (ls :< a)
instance      Layered   (ls :< a)

instance Monad m => LayerConstructor m (ls :< a) where
    constructLayer = return ∘ wrap' ; {-# INLINE constructLayer #-}

instance Castable (Unwrapped (ls :< a)) (Unwrapped (ls' :< a')) => Castable (ls :< a) (ls' :< a') where
    cast = wrapped %~ cast ; {-# INLINE cast #-}


-- Attributes

type instance                                      Attr     a (ls :< t) = Attr a (Unwrapped (ls :< t))
instance HasAttr     a (Unwrapped (ls :< t)) => HasAttr     a (ls :< t) where attr      = wrapped' ∘∘  attr      ; {-# INLINE attr      #-}
instance MayHaveAttr a (Unwrapped (ls :< t)) => MayHaveAttr a (ls :< t) where checkAttr = wrapped' ∘∘∘ checkAttr ; {-# INLINE checkAttr #-}



---------------------------
-- === Native layers === --
---------------------------

-- === Layout-specific === --

data Type  = Type  deriving (Show, Eq, Ord)
data Succs = Succs deriving (Show, Eq, Ord)


-- === Universal === --

-- Note layer
data Note = Note deriving (Show, Eq, Ord)
type instance LayerData layout Note t = String
instance Monad m => Maker m (Layer layout Note a) where make = return $ Layer ""