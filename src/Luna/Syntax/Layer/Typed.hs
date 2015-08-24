{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Luna.Syntax.Layer.Typed where

import Flowbox.Prelude

import Luna.Syntax.AST
import Luna.Syntax.Builder
import Luna.Syntax.Layer
import Data.Cata

-- === Typed ===

data Typed a t = Typed t (a t)
type instance ASTOf (Typed a) = ASTOf a

deriving instance (Show t, Show (a t)) => Show (Typed a t)


instance HasAST a ast => HasAST (Typed a) ast where ast = inner . ast

instance Layer Typed where
    inner = lens (\(Typed _ a) -> a) (\(Typed t _) a -> Typed t a)

instance (LayeredStarBuilder m t, LayerGen (Mu t) m a) => LayerGen (Mu t) m (Typed a) where
    genLayers a = Typed <$> getStar <*> genLayers a