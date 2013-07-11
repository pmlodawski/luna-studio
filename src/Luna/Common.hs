module Luna.Common(
Graph(..),
Node(..),
NodeDef(..),
) where

import Data.GraphViz.Attributes (Labellable, toLabelValue)
import qualified Data.Graph.Inductive as DG
import qualified Luna.Edge 			  as Edge
import qualified Luna.DefaultValue 	  as DefaultValue
import Data.Map(Map)
import Data.MultiMap(MultiMap)

--type Graph = DG.Gr Node Edge.Edge

data Graph = Graph {
	repr      :: DG.Gr Node Edge.Edge,
	types     :: Map 	  String DG.Node,
	calls     :: Map 	  String DG.Node,
	classes   :: Map 	  String DG.Node,
	functions :: MultiMap String DG.Node,
	packages  :: Map 	  String DG.Node
} --deriving (Show)

data Node = TypeNode    { name :: String }
		  | CallNode    { name :: String }
		  | ClassNode   { name :: String, def :: NodeDef }
		  | FunctionNode{ name :: String, def :: NodeDef }
		  | PackageNode { name :: String, def :: NodeDef }
		  | DefaultNode { value :: DefaultValue.DefaultValue }
		  --deriving (Show)

--instance Labellable Node where
--	toLabelValue = toLabelValue . show

-----------------------------------------------------------------

noPorts :: [String]
noPorts = []

data NodeDef = NodeDef {
	inputs 	:: [String],
	outputs :: [String],
	imports :: [String],
	graph	:: Graph,
	libID	:: Int
} --deriving (Show)