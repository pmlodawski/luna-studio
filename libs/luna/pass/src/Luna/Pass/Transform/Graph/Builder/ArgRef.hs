---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Luna.Pass.Transform.Graph.Builder.ArgRef where

import           Data.Either            (lefts, rights)

import           Flowbox.Prelude
import qualified Luna.Syntax.Graph.Node as Node
import           Luna.Syntax.Graph.Port (DstPort, SrcPort)
import           Luna.Syntax.Graph.Tag  (TExpr)



type NodeRef    = (Node.ID, SrcPort, DstPort)
type DefaultRef = (DstPort, TExpr ())
type ArgRef     = Either NodeRef DefaultRef


mkNode :: NodeRef -> ArgRef
mkNode = Left

mkDefault :: DefaultRef -> ArgRef
mkDefault = Right

defaults :: [ArgRef] -> [DefaultRef]
defaults = rights

nodes :: [ArgRef] -> [NodeRef]
nodes = lefts
