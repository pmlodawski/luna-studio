module Empire.Data.AST where

import Prologue
import Luna.Syntax.Repr.Graph
import Luna.Syntax.AST.Term
import Luna.Syntax.AST.Typed
import Luna.Syntax.Layer.Labeled
import Data.Layer.Coat
import Empire.API.Data.NodeMeta (NodeMeta)

type ASTNode = (Labeled2 (Maybe NodeMeta) (Typed (Ref Edge) (SuccTracking (Coat (Draft (Ref Edge))))))
type AST = Graph ASTNode DoubleArc
