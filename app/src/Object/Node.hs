module Object.Node where

import           Control.Applicative
import           Control.Lens
import           Data.Dynamic
import           Data.Monoid
import           Data.Maybe    ( isJust, catMaybes )

import           Utils.Wrapper
import           Utils.PrettyPrinter
import           Object.Dynamic
import           Object.Object

type NodeId = ID

data Node = Node { _ident    :: NodeId
                 , _selected :: Bool
                 , _position :: Vector2 Int
                 } deriving (Eq, Show, Typeable)

type NodeCollection   = [Node]
type NodeIdCollection = [NodeId]

makeLenses ''Node

instance PrettyPrinter Node where
    display (Node id sel pos) = "n( " <> show id <> " " <> show sel <> " " <> display pos <> " )"

instance Selectable Node where
    setSelected n selected = n { _selected = selected }
    isSelected  n          = _selected n


isNode :: Object Dynamic -> Bool
isNode obj = isJust (unpackDynamic obj :: Maybe Node)


getNodesAt :: Vector2 Int -> Double -> NodeCollection -> NodeCollection
getNodesAt pos camFactor nodes = filter closeEnough nodes where
    radiusSquared    = 900.0 * camFactor
    closeEnough node = (fromIntegral distSquared) < radiusSquared where
        distSquared  = (dist ^. x) ^ 2 + (dist ^. y) ^ 2
        dist         = (node ^. position - pos)
