{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Empire.Data.AST where

import           Prologue

import Data.Graph                               (Node, Link, Ref, Edge)
import Luna.Syntax.Model.Network.Builder.Term   (NetGraph, NetNode, NetLayers)
import Empire.API.Data.NodeMeta                 (NodeMeta)
import           Luna.Evaluation.Runtime        (Static)
import           Luna.Syntax.Model.Network.Term (Draft)

type ASTNode       = NetNode NodeMeta
type ASTEdge       = Link (NetNode NodeMeta)
type AST           = NetGraph NodeMeta

type NodeRef       = Ref Node ASTNode
type EdgeRef       = Ref Edge ASTEdge

type UncoveredNode = Draft Static $ NetLayers NodeMeta
