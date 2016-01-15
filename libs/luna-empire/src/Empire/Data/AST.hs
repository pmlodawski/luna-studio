module Empire.Data.AST where

import Prologue
import Data.Layer.Coat

import Luna.Syntax.Repr.Graph
import Luna.Syntax.AST.Term
import Luna.Syntax.AST.Typed
import Luna.Syntax.Layer.Labeled

import Empire.API.Data.NodeMeta (NodeMeta)
import Empire.Data.WithMeta     (WithMeta)

type ASTNode = (WithMeta (Maybe NodeMeta) (Labeled2 Int (Typed (Ref Edge) (SuccTracking (Coat (Draft (Ref Edge)))))))
type AST = Graph ASTNode DoubleArc
