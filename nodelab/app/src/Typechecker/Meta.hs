{-# LANGUAGE UndecidableInstances #-}

module Typechecker.Meta where

import           Utils.PreludePlus


import           Luna.Syntax.Graph.Builder.State (BldrState(..))
import           Luna.Syntax.Graph.Builder
import           Luna.Syntax.Graph
import           Luna.Syntax.Decl
import           Luna.Syntax.Term

import           Control.Monad.State
import           Typechecker.Label

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


type GraphMeta         = HomoNet (Label Meta) Term
type FunctionGraphMeta = Function  GraphMeta
type StateGraphMeta    = BldrState GraphMeta

