---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Luna.Common(
Graph   (..),
Node    (..),
NodeDef (..),
) where


import qualified Data.Graph.Inductive as DG
import           Luna.Edge 			    (Edge)
import           Luna.DefaultValue 	    (DefaultValue)
import           Data.Map               (Map)
import qualified Data.MultiMap        as MultiMap
import           Data.MultiMap          (MultiMap)
import qualified Luna.Library         as Library

data Graph = Graph {
	repr      :: DG.Gr Node Edge,
	types     :: Map 	  String DG.Node,
	calls     :: Map 	  String DG.Node,
	classes   :: Map 	  String DG.Node,
	functions :: MultiMap String DG.Node,
	packages  :: Map 	  String DG.Node
} deriving (Show)

-----------------------------------------------------------------

data Node = TypeNode    { name  :: String }
		  | CallNode    { name  :: String }
		  | ClassNode   { name  :: String, def :: NodeDef }
		  | FunctionNode{ name  :: String, def :: NodeDef }
		  | PackageNode { name  :: String, def :: NodeDef }
		  | DefaultNode { value :: DefaultValue }
		  deriving (Show)


-- FIXME[wd] move the following instance to the right place
instance (Show k, Show a) => Show (MultiMap k a) where
    show a = show $ MultiMap.toMap a


-----------------------------------------------------------------

data NodeDef = NodeDef {
	inputs 	:: [String],
	outputs :: [String],
	imports :: [String],
	graph	:: Graph,
	libID	:: Library.LibID
} deriving (Show)