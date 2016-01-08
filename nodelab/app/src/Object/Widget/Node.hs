{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}

module Object.Widget.Node where

import Utils.PreludePlus
import Utils.Vector
import Object.UITypes
import qualified Empire.API.Data.Node as N
import qualified Empire.API.Data.NodeMeta as NM
import Data.Fixed

import Object.Widget
import Utils.CtxDynamic
import Event.Mouse      (MouseButton(..))
import Data.Aeson (ToJSON)

data Node = Node { _nodeId     :: Int
                 , _controls   :: [Maybe WidgetId]
                 , _ports      :: [WidgetId]
                 , _position   :: Position
                 , _zPos       :: Double
                 , _expression :: Text
                 , _value      :: Text
                 , _isExpanded :: Bool
                 , _isSelected :: Bool
                 , _isFocused  :: Bool
                 } deriving (Eq, Show, Typeable, Generic)

makeLenses ''Node
instance ToJSON Node

node :: N.Node -> Node
node n = Node (n ^. N.nodeId) [] [] (uncurry Vector2 $ n ^. N.nodeMeta ^. NM.position) 0.0 (n ^. N.expression) "()" False False False

instance IsDisplayObject Node where
    widgetPosition = position
    widgetSize     = lens get set where
        get _      = Vector2 60.0 60.0
        set w _    = w

data PendingNode = PendingNode { _pendingExpression :: Text
                               , _pendingPosition   :: Position
                               } deriving (Eq, Show, Typeable, Generic)

makeLenses ''PendingNode
instance ToJSON PendingNode

instance IsDisplayObject PendingNode where
    widgetPosition = pendingPosition
    widgetSize     = lens get set where
        get _      = Vector2 60.0 60.0
        set w _    = w
