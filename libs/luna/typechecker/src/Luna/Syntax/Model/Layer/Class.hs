{-# LANGUAGE UndecidableInstances #-}

module Luna.Syntax.Model.Layer.Class where

import Prologue

import Data.Attribute
import Data.Construction
import Data.Layer.Cover
import Type.Bool
import Luna.Syntax.AST.Term (LayoutType)



-------------------------
-- === Tagged data === --
-------------------------

newtype Tagged t a = Tagged a deriving (Show, Eq, Ord, Functor, Traversable, Foldable)
makeWrapped ''Tagged

instance Castable a a' => Castable (Tagged t a) (Tagged t a') where cast = wrapped %~ cast ; {-# INLINE cast #-}


--------------------
-- === Layers === --
--------------------

-- === Definitions === --

--powinnismy zmienic nazwe layer na attached , ktory attachuje layer
--albo nie - do przemyslenia

--tak czy siak, podczas konstrukcji warstwy musimy wiedziec czy do wartwa do grafu czy do np. plakiej struktury, teraz do make tego nei przekazujemy
data Layer t a = Layer (AttachedData (Layer t a)) a
type family LayerData t d a


--newtype Layer layout t a = Layer (LayerData layout t a)
-- === Utils === --

type family AttachedData l where AttachedData (Layer t a) = Tagged t (LayerData (LayoutType (Uncovered a)) t (Uncovered a))


-- === Instances === --

deriving instance (Show (AttachedData (Layer t a)), Show a) => Show (Layer t a)

type instance Unlayered (Layer t a) = a
instance      Layered (Layer t a) where
    layered = lens (\(Layer _ a) -> a) (\(Layer d _) a -> Layer d a) ; {-# INLINE layered #-}

instance (Maker m (AttachedData (Layer t a)), Functor m)
      => LayerConstructor m (Layer t a) where
    constructLayer a = flip Layer a <$> make ; {-# INLINE constructLayer #-}

instance (Castable a a', Castable (AttachedData (Layer t a)) (AttachedData (Layer t' a')))
      => Castable (Layer t a) (Layer t' a') where
    cast (Layer d a) = Layer (cast d) (cast a) ; {-# INLINE cast #-}

-- Attributes

type instance Attr a (Layer t l) = If (a == t) (LayerData (LayoutType (Uncovered l)) t (Uncovered l)) (Attr a l)
instance {-# OVERLAPPABLE #-} (Attr a (Layer t l) ~ Attr a (Unlayered (Layer t l)), HasAttr     a l) => HasAttr     a (Layer t l) where attr      = layered ∘∘  attr      ; {-# INLINE attr      #-}
instance {-# OVERLAPPABLE #-} (Attr a (Layer t l) ~ Attr a (Unlayered (Layer t l)), MayHaveAttr a l) => MayHaveAttr a (Layer t l) where checkAttr = layered ∘∘∘ checkAttr ; {-# INLINE checkAttr #-}
instance {-# OVERLAPPABLE #-} HasAttr     a (Layer a l) where attr _ = lens (\(Layer d _) -> d) (\(Layer _ a) d -> Layer d a) ∘ wrapped'                                  ; {-# INLINE attr      #-}
instance {-# OVERLAPPABLE #-} MayHaveAttr a (Layer a l)



--------------------
-- === Shell === ---
--------------------

data (layers :: [*]) :< (a :: [*] -> *) = Shell (ShellStrcture layers (a layers))

type family ShellStrcture ls a where 
    ShellStrcture '[]       a = Cover a
    ShellStrcture (l ': ls) a = Layer l (ShellStrcture ls a)


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
