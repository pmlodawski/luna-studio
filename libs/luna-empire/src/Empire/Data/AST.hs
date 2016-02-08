{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Empire.Data.AST where

import           Prologue

import Luna.Syntax.Model.Graph                (Node, Link, Ref)
import Luna.Syntax.Model.Network.Builder.Term (NetGraph, NetNode)
import Empire.API.Data.NodeMeta               (NodeMeta)

type ASTNode       = Node (NetNode NodeMeta)
type ASTEdge       = Link (NetNode NodeMeta)
type AST           = NetGraph NodeMeta

type NodeRef       = Ref ASTNode
type EdgeRef       = Ref ASTEdge
