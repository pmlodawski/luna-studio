module Object.Node where

import           Control.Applicative
import           Control.Lens
import           Data.Dynamic
import           Data.Monoid
import           Data.Maybe    ( isJust, catMaybes )

import           JS.Camera
import           Object.Dynamic
import           Object.Object
import           Utils.Vector
import           Utils.Wrapper
import           Utils.PrettyPrinter


import qualified Data.Text.Lazy as Text
import           Data.Text.Lazy ( Text )

type NodeId = ID

data Node = Node { _nodeId     :: NodeId
                 , _selected   :: Bool
                 , _position   :: Vector2 Double
                 , _expression :: Text
                 } deriving (Eq, Show, Typeable)

type NodeCollection   = [Node]
type NodeIdCollection = [NodeId]

makeLenses ''Node

instance PrettyPrinter Node where
    display (Node ident sel pos expr) = "n(" <> display ident
                                      <> " " <> display sel
                                      <> " " <> display pos
                                      <> " " <> display expr
                                      <> ")"

instance Selectable Node where
    setSelected n selected = n { _selected = selected }
    isSelected  n          = _selected n


isNode :: Object Dynamic -> Bool
isNode obj = isJust (unpackDynamic obj :: Maybe Node)


updateNodeSelection :: NodeIdCollection -> Node -> Node
updateNodeSelection selNodeIds node = let selection = (node ^. nodeId) `elem` selNodeIds in
    node & selected .~ selection

updateNodesSelection :: NodeIdCollection -> NodeCollection -> NodeCollection
updateNodesSelection selNodeIds nodes = fmap (updateNodeSelection selNodeIds) nodes

nodeRadius    = 30.0
radiusSquared = 900.0
radiusShadow  = 21.0

getNodesAt :: Vector2 Int -> Camera -> NodeCollection -> NodeCollection
getNodesAt posScr camera nodes = filter closeEnough nodes where
    pos              = screenToWorkspace camera posScr
    closeEnough node = inRange && inRadius where
        inRange      = dist ^. x < nodeRadius && dist ^. y < nodeRadius
        inRadius     = distSquared < radiusSquared
        distSquared  = (dist ^. x) ^ 2 + (dist ^. y) ^ 2
        dist         = (node ^. position - pos)

-- TODO: Clever algorithm taking radius into account
getNodeIdsIn :: Vector2 Int -> Vector2 Int -> Camera -> NodeCollection -> NodeIdCollection
getNodeIdsIn (Vector2 x1 y1) (Vector2 x2 y2) camera nodes = (^. nodeId) <$> nodesInBounds where
    leftBottom = screenToWorkspace camera (Vector2 (min x1 x2) (max y1 y2)) - Vector2 radiusShadow radiusShadow
    rightTop   = screenToWorkspace camera (Vector2 (max x1 x2) (min y1 y2)) + Vector2 radiusShadow radiusShadow
    nodesInBounds :: NodeCollection
    nodesInBounds = filter isNodeInBounds nodes
    isNodeInBounds node = let pos = node ^. position in
                          leftBottom ^. x <= pos ^. x && pos ^. x <= rightTop ^. x &&
                          leftBottom ^. y <= pos ^. y && pos ^. y <= rightTop ^. y
