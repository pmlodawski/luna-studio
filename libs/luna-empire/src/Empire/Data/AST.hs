module Empire.Data.AST where

import Prologue
import Luna.Syntax.Repr.Graph
import Luna.Syntax.AST.Term
import Luna.Syntax.AST.Typed
import Luna.Syntax.Layer.Labeled
import Data.Layer.Coat

type AST = Graph (Labeled2 Int (Typed (Ref Edge) (SuccTracking (Coat (Draft (Ref Edge)))))) DoubleArc
