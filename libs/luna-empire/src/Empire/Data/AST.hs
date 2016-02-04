{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Empire.Data.AST where

import Prologue

import Luna.Syntax.Model.Graph       (Node, Link, Ref)
import Luna.Syntax.AST.Layout        (Static)
import Luna.Syntax.Model.Layer.Class ((:<))
import Luna.Syntax.Model.Graph.Term  (NetGraph, NetType)
{-import Luna.Syntax.Layer.Labeled-}
{-import Luna.Syntax.Network        (Network, NetworkNode)-}

{-import Empire.API.Data.NodeMeta   (NodeMeta)-}
{-import Luna.Syntax.Layer.WithMeta (WithMeta)-}

type ASTNode       = Node NetType
type ASTEdge       = Link NetType
type AST           = NetGraph

type NodeRef       = Ref ASTNode
type EdgeRef       = Ref ASTEdge
