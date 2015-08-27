{-# LANGUAGE UndecidableInstances #-}

module Typechecker.Typechecker (
    module Typechecker.Typechecker,
    module X
) where


import           Flowbox.Prelude hiding (Cons, cons)

import           Control.Monad.State

import           Luna.Syntax.Builder.Graph
import           Luna.Syntax.Builder
import           Luna.Syntax.Layer.Labeled
import           Luna.Syntax.Layer.Typed
import           Luna.Syntax.AST.Term
import           Luna.Syntax.AST.Decl.Function
import           Luna.Repr.Styles

import           Typechecker.Meta  as X


type LabeledMeta          = Labeled Meta (Typed Term)
type GraphMeta            = HomoGraph ArcPtr LabeledMeta
type GraphRefMeta         = Arc              LabeledMeta

type FunctionGraphMeta    = Function  GraphMeta
type StateGraphMeta       = BldrState GraphMeta

type RefFunctionGraphMeta = (GraphRefMeta, FunctionGraphMeta)


instance Repr s FunctionGraphMeta where repr = fromString . show

evalFunctionBuilderState :: Default d => GraphStarBuilderT s g (StateT d Identity) a -> BldrState g -> a
execFunctionBuilderState :: Default d => GraphStarBuilderT s g (StateT d Identity) a -> BldrState g -> Function g
runFunctionBuilderState  :: Default d => GraphStarBuilderT s g (StateT d Identity) a -> BldrState g -> (a, Function g)


evalFunctionBuilderState = (runIdentity . flip evalStateT def) .: evalFunctionBuilderT
execFunctionBuilderState = (runIdentity . flip evalStateT def) .: execFunctionBuilderT
runFunctionBuilderState  = (runIdentity . flip evalStateT def) .: runFunctionBuilderT

