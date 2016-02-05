{-# LANGUAGE NoMonomorphismRestriction #-}

module Luna.Syntax.Model.Network.Graph (module Luna.Syntax.Model.Network.Graph, module X) where

import Prelude.Luna

import           Luna.Syntax.Model.Graph as X hiding (nodes, edges)
import qualified Luna.Syntax.Model.Graph as Graph
import           Luna.Syntax.Model.Network.Term
import           Luna.Runtime.Model


nodes :: BiCastable n (Draft Dynamic ls) => Lens' (Graph n e) [Draft Dynamic ls]
nodes = Graph.nodes ∘ casted

edges :: BiCastable e (Link (Draft Dynamic ls)) => Lens' (Graph n e) [Link (Draft Dynamic ls)]
edges = Graph.edges ∘ casted
