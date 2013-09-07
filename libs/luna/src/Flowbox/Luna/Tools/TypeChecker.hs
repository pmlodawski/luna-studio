---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Luna.Tools.TypeChecker(
    --typeCheck
) where

import           Flowbox.Prelude

--import qualified Data.Graph.Inductive as DG
--import qualified Data.Map     as Map
--import           Data.Map       (Map)

--import qualified Flowbox.Luna.Network.Def.DefManager as DefManager
--import           Flowbox.Luna.Network.Def.DefManager   (DefManager)
--import qualified Flowbox.Luna.Network.Graph.Graph   as Graph
--import           Flowbox.Luna.Network.Graph.Graph     (Graph)
--import qualified Flowbox.Luna.Network.Graph.Node    as Node
--import           Flowbox.Luna.Network.Graph.Node      (Node)
---- import qualified Flowbox.Luna.NodeDef as NodeDef


--typeCheck :: Graph -> DefManager -> Map DG.Node (Maybe Node)
--typeCheck graph manager = m where -- TODO[PM] implement typeCheck
--    nodes = DG.labNodes $ Graph.repr graph
--    callNodes = filter (\(_, node) -> case node of 
--                                 Node.CallNode _ -> True;
--                                 _               -> False) nodes
--    types = map (\(nid, node) -> 
--                  (nid, DefManager.nodesByCName (Node.name node) manager)) callNodes
--    m = Map.fromList types
