---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Luna.Common.Graph(
    newNodes,
    lnodeById,
    nodeById
) where

import qualified Data.Graph.Inductive as DG

newNodes :: DG.Graph gr => gr a b -> [DG.Node]
newNodes gr = [n+1..] where (_,n) = DG.nodeRange $ gr

lnodeById :: DG.Graph gr => gr a b -> DG.Node -> DG.LNode a
lnodeById graph nid = DG.labNode' $ DG.context graph nid

nodeById :: DG.Graph gr => gr a b -> DG.Node -> a
nodeById graph nid = snd $ lnodeById graph nid