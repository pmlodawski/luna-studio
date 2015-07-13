module Object.Node where

import           Control.Applicative
import           Control.Lens
import           Data.Dynamic
import           Data.Monoid
import           Data.Maybe    ( isJust, catMaybes )

import           JS.Utils      as Utils
import           Utils.Vector
import           Utils.Wrapper
import           Utils.PrettyPrinter
import           Object.Dynamic
import           Object.Object

type NodeId = ID

data Node = Node { _ident    :: NodeId
                 , _selected :: Bool
                 , _position :: Vector2 Double
                 } deriving (Eq, Show, Typeable)

type NodeCollection   = [Node]
type NodeIdCollection = [NodeId]

makeLenses ''Node

instance PrettyPrinter Node where
    display (Node ident sel pos) = "n( " <> display ident <>
                                   " "   <> display sel <>
                                   " "   <> display pos <> " )"

instance Selectable Node where
    setSelected n selected = n { _selected = selected }
    isSelected  n          = _selected n


isNode :: Object Dynamic -> Bool
isNode obj = isJust (unpackDynamic obj :: Maybe Node)


updateNodeSelection :: NodeIdCollection -> Node -> Node
updateNodeSelection selNodeIds node = let selection = (node ^. ident) `elem` selNodeIds in
    node & selected .~ selection

updateNodesSelection :: NodeIdCollection -> NodeCollection -> NodeCollection
updateNodesSelection selNodeIds nodes = fmap (updateNodeSelection selNodeIds) nodes


-- TODO: optimization - filter out nodes just by x and y
getNodesAt :: Vector2 Int -> Utils.Camera -> NodeCollection -> NodeCollection
getNodesAt posScr camera nodes = filter closeEnough nodes where
    pos              = Utils.screenToWorkspace camera posScr
    radiusSquared    = 900.0
    closeEnough node = distSquared < radiusSquared where
        distSquared  = (dist ^. x) ^ 2 + (dist ^. y) ^ 2
        dist         = (node ^. position - pos)

getNodeIdsIn :: Vector2 Int -> Vector2 Int -> Utils.Camera -> NodeCollection -> NodeIdCollection
getNodeIdsIn (Vector2 x1 y1) (Vector2 x2 y2) camera nodes = (^. ident) <$> nodesInBounds where
    leftBottom = Utils.screenToWorkspace camera $ Vector2 (min x1 x2) (max y1 y2)
    rightTop   = Utils.screenToWorkspace camera $ Vector2 (max x1 x2) (min y1 y2)
    nodesInBounds :: NodeCollection
    nodesInBounds = filter isNodeInBounds nodes
    isNodeInBounds node = let pos = node ^. position in
                          leftBottom ^. x <= pos ^. x && pos ^. x <= rightTop ^. x &&
                          leftBottom ^. y <= pos ^. y && pos ^. y <= rightTop ^. y
