module Luna.Node(
Node(..),
NodeDef(..),
Graph,
) where

import Data.GraphViz.Attributes (Labellable, toLabelValue)
import qualified Data.Graph.Inductive as DG
import qualified Luna.Edge as Edge
import qualified Luna.DefaultValue as DefaultValue

type Graph = DG.Gr Node Edge.Edge

data Node = TypeNode    { name :: String }
		  | CallNode    { name :: String }
		  | ClassNode   { name :: String, def :: NodeDef }
		  | FunctionNode{ name :: String, def :: NodeDef }
		  | PackageNode { name :: String, def :: NodeDef }
		  | DefaultNode { defValue :: DefaultValue.DefaultValue }
		  deriving (Show)

instance Labellable Node where
	toLabelValue = toLabelValue . show



noPorts :: [String]
noPorts = []

data NodeDef = NodeDef {
	inputs 	:: [String],
	outputs :: [String],
	imports :: [String],
	graph	:: Graph,
	libID	:: Int
} deriving (Show)