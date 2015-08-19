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



type GraphMeta         = HomoNet (Label Meta) Term
type FunctionGraphMeta = Function  GraphMeta
type StateGraphMeta    = BldrState GraphMeta


evalFunctionBuilderState :: Default s => GraphBuilderT g (StateT s Identity) a -> BldrState g -> a
execFunctionBuilderState :: Default s => GraphBuilderT g (StateT s Identity) a -> BldrState g -> Function g
runFunctionBuilderState  :: Default s => GraphBuilderT g (StateT s Identity) a -> BldrState g -> (a, Function g)

-- evalFunctionBuilderState bldr s = runIdentity $ flip evalStateT def $ evalFunctionBuilderT bldr s
-- execFunctionBuilderState bldr s = runIdentity $ flip evalStateT def $ execFunctionBuilderT bldr s
-- runFunctionBuilderState  bldr s = runIdentity $ flip evalStateT def $ runFunctionBuilderT  bldr s

evalFunctionBuilderState = (runIdentity . flip evalStateT def) .: evalFunctionBuilderT
execFunctionBuilderState = (runIdentity . flip evalStateT def) .: execFunctionBuilderT
runFunctionBuilderState  = (runIdentity . flip evalStateT def) .: runFunctionBuilderT

