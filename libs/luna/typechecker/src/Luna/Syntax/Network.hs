module Luna.Syntax.Network where

import Data.Layer.Coat            (Coat)
import Luna.Syntax.AST.Term       (Draft)
import Luna.Syntax.AST.Typed      (Typed)
import Luna.Syntax.Layer.Labeled  (Labeled2)
import Luna.Syntax.Layer.WithMeta (WithMeta)
import Luna.Syntax.Repr.Graph     (DoubleArc, Edge, Graph, Ref, SuccTracking)


type NetworkNode l m = (WithMeta m (Labeled2 l (Typed (Ref Edge) (SuccTracking (Coat (Draft (Ref Edge)))))))
type Network l m = Graph (NetworkNode l m) DoubleArc
