module Luna.Syntax.Layer where

import Flowbox.Prelude

import Luna.Syntax.AST.Term
import Luna.Syntax.AST

class Layer l where
    inner :: Lens (l a t) (l a' t) (a t) (a' t)

class Monad m => LayerGen t m l where
    genLayers :: ASTOf l t -> m (l t)

instance Monad m => LayerGen t m Val   where genLayers = return
instance Monad m => LayerGen t m Thunk where genLayers = return
instance Monad m => LayerGen t m Term  where genLayers = return
instance Monad m => LayerGen t m Draft where genLayers = return