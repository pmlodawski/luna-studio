{-# LANGUAGE ExistentialQuantification #-}

module Object.Widget.Node where

import Utils.PreludePlus
import Utils.Vector
import Object.UITypes
import Data.Fixed

import Object.Widget
import Utils.CtxDynamic
import Event.Mouse      (MouseButton(..))
import Data.Aeson (ToJSON)

data Node = Node { _nodeId     :: Int
                 , _controls   :: [WidgetId]
                 , _ports      :: [WidgetId]
                 , _position   :: Position
                 , _expression :: Text
                 , _value      :: Text
                 , _isExpanded :: Bool
                 , _isSelected :: Bool
                 , _isFocused  :: Bool
                 } deriving (Eq, Show, Typeable, Generic)

makeLenses ''Node
instance ToJSON Node

instance IsDisplayObject Node where
    widgetPosition = position

data PendingNode = PendingNode { _pendingExpression :: Text
                               , _pendingPosition   :: Position
                               } deriving (Eq, Show, Typeable, Generic)

makeLenses ''PendingNode
instance ToJSON PendingNode

instance IsDisplayObject PendingNode where
    widgetPosition = pendingPosition
