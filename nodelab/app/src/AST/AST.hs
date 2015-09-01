{-# LANGUAGE UndecidableInstances #-}

module AST.AST (
    module AST.AST,
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

import           AST.Meta  as X


type GraphStarBuilderState s g d = GraphStarBuilderT s g (StateT d Identity)

runGraphState  :: Default d => GraphStarBuilderState s g d a -> BldrState g -> (a, g)
runGraphState  = (runIdentity . flip evalStateT def) .: runGraphT

rebuild :: g -> BldrState g
rebuild f = BldrState [] $ f
