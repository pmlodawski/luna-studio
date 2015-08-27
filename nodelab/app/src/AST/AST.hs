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


type LabeledMeta          = Labeled Meta (Typed Term)
type GraphMeta            = HomoGraph ArcPtr LabeledMeta
type GraphRefMeta         = Arc              LabeledMeta

type StateGraphMeta       = BldrState GraphMeta

type RefFunctionGraphMeta = (GraphRefMeta, GraphMeta)


instance Repr s GraphMeta where repr = fromString . show

type GraphStarBuilderState s g d = GraphStarBuilderT s g (StateT d Identity)

runGraphState  :: Default d => GraphStarBuilderState s g d a -> BldrState g -> (a, g)
runGraphState  = (runIdentity . flip evalStateT def) .: runGraphT
