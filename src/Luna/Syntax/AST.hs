{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE TypeFamilies           #-}

module Luna.Syntax.AST where

import Flowbox.Prelude
import Luna.Syntax.Term


-- === ASTOf ===

type family ASTOf (a :: * -> *) :: * -> *

type instance ASTOf Val   = Val
type instance ASTOf Thunk = Thunk
type instance ASTOf Term  = Term

-- === LayerGen ===

class Monad m => LayerGen t m l where
    genLayers :: ASTOf l t -> m (l t)

instance Monad m => LayerGen t m Val   where genLayers = return
instance Monad m => LayerGen t m Thunk where genLayers = return
instance Monad m => LayerGen t m Term  where genLayers = return

-- === HasAST ===

class HasAST l ast | l -> ast where
    ast :: Lens' (l t) (ast t)

instance HasAST Val   Val   where ast = id
instance HasAST Thunk Thunk where ast = id
instance HasAST Term  Term  where ast = id

-- === Layer ===

class Layer l where
    inner :: Lens (l a t) (l a' t) (a t) (a' t)

