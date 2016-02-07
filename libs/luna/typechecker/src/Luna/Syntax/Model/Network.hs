module Luna.Syntax.Model.Network (module Luna.Syntax.Model.Network, module X) where

import           Prelude.Luna

import           Luna.Evaluation.Runtime
import qualified Luna.Syntax.Model.Graph         as Graph
import           Luna.Syntax.Model.Graph         as X hiding (nodes, edges)
import           Luna.Syntax.Model.Network.Class as X
import           Luna.Syntax.Model.Network.Term


nodes :: BiCastable n (Draft Dynamic ls) => Lens' (Graph n e) [Draft Dynamic ls]
nodes = Graph.nodes ∘ casted

edges :: BiCastable e (Link (Draft Dynamic ls)) => Lens' (Graph n e) [Link (Draft Dynamic ls)]
edges = Graph.edges ∘ casted
