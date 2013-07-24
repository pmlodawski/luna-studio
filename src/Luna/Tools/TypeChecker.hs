---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Luna.Tools.TypeChecker(
    typeCheck
) where

import qualified Data.Graph.Inductive as DG
import qualified Data.Map     as Map
import           Data.Map       (Map)

import qualified Luna.DefManager as DefManager
import           Luna.DefManager   (DefManager)
import qualified Luna.Graph   as Graph
import           Luna.Graph     (Graph)
import qualified Luna.Node    as Node
import           Luna.Node      (Node)
-- import qualified Luna.NodeDef as NodeDef


typeCheck :: Graph -> DefManager -> Map DG.Node (Maybe Node)
typeCheck graph manager = m where -- TODO[PM] implement typeCheck
    nodes = DG.labNodes $ Graph.repr graph
    callNodes = filter (\(_, node) -> case node of 
                                 Node.CallNode _ -> True;
                                 _               -> False) nodes
    types = map (\(nid, node) -> 
                  (nid, DefManager.nodesByCName (Node.name node) manager)) callNodes
    m = Map.fromList types
