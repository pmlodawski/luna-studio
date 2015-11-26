module Luna.Syntax.Symbol.Network where

import Prologue

import Data.Layer.Coat           (Coat)
import Luna.Syntax.AST.Term      (Draft)
import Luna.Syntax.AST.Typed     (Typed)
import Luna.Syntax.Layer.Labeled (Labeled2)
import Luna.Syntax.Repr.Graph    (DoubleArc, Edge, Graph, Ref, SuccTracking)


type Network = Graph (Labeled2 Int (Typed (Ref Edge) (SuccTracking (Coat (Draft (Ref Edge)))))) DoubleArc
