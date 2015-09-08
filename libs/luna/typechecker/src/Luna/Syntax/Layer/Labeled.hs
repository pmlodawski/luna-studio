{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Luna.Syntax.Layer.Labeled where

import Flowbox.Prelude
import Luna.Syntax.Layer
import Luna.Syntax.AST
import Data.Variants

data Labeled l a t = Labeled l (a t)
type instance ASTOf (Labeled l a) = ASTOf a


class LabBuilder m l where
    mkLabel :: m l

class HasLabel l a | a -> l where
    label :: Lens' a l

type instance Variants (Labeled l a t) = Variants (a t)
--instance Record (Labeled )
-- instances

deriving instance (Show l, Show (a t)) => Show (Labeled l a t)

instance HasLabel l (Labeled l a t) where
    label = lens (\(Labeled l _) -> l) (\(Labeled _ a) l -> Labeled l a)

instance {-# OVERLAPPABLE #-} (Monad m, Default a) => LabBuilder m a where
    mkLabel = return def

--instance HasAST (a t) ast => HasAST (Labeled l a t) ast where ast = undefined
instance HasAST a ast => HasAST (Labeled l a) ast where ast = inner . ast


--instance (ASTGen ast m (a t), LabBuilder m l, Applicative m) => ASTGen ast m (Labeled l a t) where
--    genAST a = Labeled <$> mkLabel <*> genAST a

instance Layer (Labeled l) where
    inner = lens (\(Labeled _ a) -> a) (\(Labeled l _) a -> Labeled l a)

instance (LayerGen t m b, LabBuilder m l) => LayerGen t m (Labeled l b) where
    genLayers a = Labeled <$> mkLabel <*> genLayers a
