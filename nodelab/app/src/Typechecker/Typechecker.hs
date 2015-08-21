{-# LANGUAGE UndecidableInstances #-}

module Typechecker.Typechecker (
    module Typechecker.Typechecker,
    module X
) where

import           Utils.PreludePlus

import           Control.Monad.State

import           Luna.Syntax.Graph.Builder.State (BldrState, GraphBuilder, GraphBuilderT)
import           Luna.Syntax.Graph.Builder
import           Luna.Syntax.Graph
import           Luna.Syntax.Decl
import           Luna.Syntax.Term

import           Typechecker.Label as X
import           Typechecker.Meta  as X



type GraphMeta            = HomoNet  (Label Meta) Term
type GraphRefMeta         = GraphRef (Label Meta) Term

type FunctionGraphMeta    = Function  GraphMeta
type StateGraphMeta       = BldrState GraphMeta

type RefFunctionGraphMeta = (GraphRefMeta, FunctionGraphMeta)


evalFunctionBuilderState :: Default s => GraphBuilderT g (StateT s Identity) a -> BldrState g -> a
execFunctionBuilderState :: Default s => GraphBuilderT g (StateT s Identity) a -> BldrState g -> Function g
runFunctionBuilderState  :: Default s => GraphBuilderT g (StateT s Identity) a -> BldrState g -> (a, Function g)

evalFunctionBuilderState = (runIdentity . flip evalStateT def) .: evalFunctionBuilderT
execFunctionBuilderState = (runIdentity . flip evalStateT def) .: execFunctionBuilderT
runFunctionBuilderState  = (runIdentity . flip evalStateT def) .: runFunctionBuilderT

