---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Luna.DefManager(
DefManager(..),
empty,
library,
nodesByCName
) where

import qualified Data.List.Split as Split
import qualified Luna.Graph   as Graph
import           Luna.Graph     (Graph)
import qualified Luna.Node    as Node
import           Luna.Node      (Node)
import qualified Luna.NodeDef as NodeDef
import qualified Data.Map     as Map
import           Data.Map       (Map)
import qualified Luna.Library as Library
import           Luna.Library   (Library)


data DefManager = DefManager{
    libraries :: Map Library.LibID Library,
    defs      :: Graph
} deriving (Show)

empty :: DefManager
empty = DefManager Map.empty Graph.empty

library :: Node -> DefManager -> Maybe Library
library node manager = Map.lookup (NodeDef.libID $ Node.def node) $ libraries manager

nodesByCName :: String -> DefManager -> [Node]
nodesByCName cname manager = node where
    node = nodesByCName' (Split.splitOn "." cname) (defs manager)

nodesByCName' :: [String] -> Graph -> [Node]
nodesByCName' [cname] graph = nodes where
    nodes = (Graph.childrenByName cname graph)
nodesByCName' (cname_head:cname_tail) graph = nodes where
    children = Graph.childrenByName cname_head graph
    graphs = map (NodeDef.graph . Node.def) children
    nodes = concat $ map (nodesByCName' cname_tail) graphs


