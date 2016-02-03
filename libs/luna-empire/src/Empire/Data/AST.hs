module Empire.Data.AST where

import Prologue
import Data.Layer.Coat

import Luna.Syntax.Repr.Graph
import Luna.Syntax.AST.Term
import Luna.Syntax.AST.Typed
import Luna.Syntax.Layer.Labeled
import Luna.Syntax.Network        (Network, NetworkNode)

import Empire.API.Data.NodeMeta   (NodeMeta)
import Luna.Syntax.Layer.WithMeta (WithMeta)

type ASTNode = NetworkNode Int (Maybe NodeMeta)
type AST = Network Int (Maybe NodeMeta)
