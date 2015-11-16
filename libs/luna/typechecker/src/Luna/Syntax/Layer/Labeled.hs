{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Luna.Syntax.Layer.Labeled where

import Prologue
import Luna.Syntax.AST
import Data.Variants


data Labeled l a t = Labeled l (a t)
--type instance ASTOf (Labeled l a) = ASTOf a

data Labeled2 l a = Labeled2 l a deriving (Show, Functor, Traversable, Foldable)

--type instance ASTOf (Labeled2 l a) = ASTOf a

type instance Unlayered (Labeled2 l a) = a

instance (Monad m, LabelBuilder m l) => LayerGen m (Labeled2 l a) where
	genLayer a = Labeled2 <$> mkLabel <*> pure a

instance Layered (Labeled2 l a) where layered = lens (\(Labeled2 _ a) -> a) (\(Labeled2 l _) a -> Labeled2 l a)

--instance (Monad m, LayerStack m a, LabelBuilder m l) => LayerStack m (Labeled2 l a) where
--	genLayerStack a = Labeled2 <$> mkLabel <*> genLayerStack a

--instance 

class LabelBuilder m l where
    mkLabel :: m l

class HasLabel l a | a -> l where
    label :: Lens' a l

type instance Variants (Labeled l a t) = Variants (a t)
--instance Record (Labeled )
-- instances

deriving instance (Show l, Show (a t)) => Show (Labeled l a t)

instance HasLabel l (Labeled l a t) where
    label = lens (\(Labeled l _) -> l) (\(Labeled _ a) l -> Labeled l a)

instance {-# OVERLAPPABLE #-} (Monad m, Default a) => LabelBuilder m a where
    mkLabel = return def

--instance HasAST (a t) ast => HasAST (Labeled l a t) ast where ast = undefined
--instance HasAST a ast => HasAST (Labeled l a) ast where ast = inner . ast


--instance (ASTGen ast m (a t), LabelBuilder m l, Applicative m) => ASTGen ast m (Labeled l a t) where
--    genAST a = Labeled <$> mkLabel <*> genAST a

--instance Layer (Labeled l) where
--    inner = lens (\(Labeled _ a) -> a) (\(Labeled l _) a -> Labeled l a)

--instance (LayerGen t m b, LabelBuilder m l) => LayerGen t m (Labeled l b) where
--    genLayers a = Labeled <$> mkLabel <*> genLayers a
