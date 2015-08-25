{-# LANGUAGE UndecidableInstances #-}

module Typechecker.Meta where

import           Utils.PreludePlus

import           Luna.Syntax.Builder
import           Luna.Syntax.Layer.Labeled

import           Control.Monad.State

data Meta = Meta Int String deriving (Show)

instance Default Meta where def = Meta 0 ""

instance {-# OVERLAPPABLE #-} (MonadState Meta m) => LabBuilder m Meta where
    mkLabel = get


withMeta :: (MonadState Meta m) => Meta -> m b -> m b
withMeta meta f = do
    old <- get
    put meta
    out <- f
    put old
    return out
