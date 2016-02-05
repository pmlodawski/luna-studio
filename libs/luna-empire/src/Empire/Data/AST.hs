{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Empire.Data.AST where

import           Prologue

import Luna.Syntax.Model.Graph       (Node, Link, Ref)
import Luna.Syntax.Model.Network.Builder.Term  (NetGraph, NetNode)
{-import Luna.Syntax.Layer.Labeled-}
{-import Luna.Syntax.Network        (Network, NetworkNode)-}

{-import Empire.API.Data.NodeMeta   (NodeMeta)-}
{-import Luna.Syntax.Layer.WithMeta (WithMeta)-}

type ASTNode       = Node NetNode
type ASTEdge       = Link NetNode
type AST           = NetGraph

type NodeRef       = Ref ASTNode
type EdgeRef       = Ref ASTEdge
