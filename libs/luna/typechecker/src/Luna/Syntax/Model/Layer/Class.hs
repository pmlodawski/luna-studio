{-# LANGUAGE UndecidableInstances #-}

module Luna.Syntax.Model.Layer.Class where

import Prologue hiding (Getter, Setter)

import Data.Attribute
import Data.Construction
import Data.Layer.Cover
import Type.Bool
import Luna.Syntax.AST.Term (LayoutType)


--------------------
-- === Layers === --
--------------------

-- === Definitions === --

type family LayerData layout d base
newtype     Layer     layout t base = Layer (LayerData layout t base) -- deriving (Show) --, Eq, Ord, Functor, Traversable, Foldable)

data Attached t a = Attached t a deriving (Show, Eq, Ord, Functor, Traversable, Foldable)


-- === Instances === --

deriving instance Show (Unwrapped (Layer l t a)) => Show (Layer l t a)

-- Wrappers

makeWrapped ''Layer
type instance Unlayered (Layer l t a) = Unwrapped (Layer l t a)
instance      Layered   (Layer l t a)

type instance Unlayered (Attached t a) = a
instance      Layered   (Attached t a) where
    layered = lens (\(Attached _ a) -> a) (\(Attached d _) a -> Attached d a) ; {-# INLINE layered #-}

-- Construction

instance (Monad m, Maker     m t) => LayerConstructor m (Attached t a) where constructLayer a = flip Attached a <$> make   ; {-# INLINE constructLayer #-}
instance (Monad m, Destroyer m t) => LayerDestructor  m (Attached t a) where destructLayer (Attached t a) = a <$ destroy t ; {-# INLINE destructLayer  #-}

-- Casting

instance (Castable a a', Castable t t') => Castable (Attached t a) (Attached t' a') where
    cast (Attached d a) = Attached (cast d) (cast a) ; {-# INLINE cast #-}

instance Castable (Unwrapped (Layer l t a)) (Unwrapped (Layer l' t' a')) => Castable (Layer l t a) (Layer l' t' a') where
    cast = wrapped %~ cast ; {-# INLINE cast #-}

-- Attributes

type instance Attr attr (Attached (Layer l t a) base) = If (attr == t) (Unwrapped (Layer l t a)) (Attr attr base)

instance {-# OVERLAPPABLE #-} (Attr  a (Attached (Layer l a' t) base) ~ Attr a base, Getter a base) 
                           => Getter a (Attached (Layer l a' t) base) where getter a (Attached _ t) = getter a t ; {-# INLINE getter #-}
instance {-# OVERLAPPABLE #-} Getter a (Attached (Layer l a  t) base) where getter _ (Attached d _) = unwrap' d  ; {-# INLINE getter #-}

instance {-# OVERLAPPABLE #-} (Attr  a (Attached (Layer l a' t) base) ~ Attr a base, Setter a base)
                           => Setter a (Attached (Layer l a' t) base) where setter a v (Attached d t) = Attached d $ setter a v t ; {-# INLINE setter #-}
instance {-# OVERLAPPABLE #-} Setter a (Attached (Layer l a  t) base) where setter _ v (Attached _ t) = Attached (Layer v) t      ; {-# INLINE setter #-}


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

instance Monad m => LayerConstructor m (ls :< a) where constructLayer = return ∘ wrap'   ; {-# INLINE constructLayer #-}
instance Monad m => LayerDestructor  m (ls :< a) where destructLayer  = return ∘ unwrap' ; {-# INLINE destructLayer  #-}

instance Castable (Unwrapped (ls :< a)) (Unwrapped (ls' :< a')) => Castable (ls :< a) (ls' :< a') where
    cast = wrapped %~ cast ; {-# INLINE cast #-}


-- Attributes

type instance                                Attr a (ls :< t) = Attr a (Unwrapped (ls :< t))
instance Getter a (Unwrapped (ls :< t)) => Getter a (ls :< t) where getter a = getter a ∘ unwrap'      ; {-# INLINE getter #-}
instance Setter a (Unwrapped (ls :< t)) => Setter a (ls :< t) where setter   = over wrapped' ∘∘ setter ; {-# INLINE setter #-}


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