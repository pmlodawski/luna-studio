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
    display (Node id sel pos) = "n( " <> show id <> " " <> show sel <> " " <> display pos <> " )"

instance Selectable Node where
    setSelected n selected = n { _selected = selected }
    isSelected  n          = _selected n


isNode :: Object Dynamic -> Bool
isNode obj = isJust (unpackDynamic obj :: Maybe Node)


getNodesAt :: Vector2 Int -> Utils.Camera -> NodeCollection -> NodeCollection
getNodesAt posScr camera nodes = filter closeEnough nodes where
    pos              = Utils.screenToWorkspace camera posScr
    radiusSquared    = 900.0
    closeEnough node = distSquared < radiusSquared where
        distSquared  = (dist ^. x) ^ 2 + (dist ^. y) ^ 2
        dist         = (node ^. position - pos)
