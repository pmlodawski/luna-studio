{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}

module Object.Widget.Node where

import           Data.Fixed
import qualified Data.Text.Lazy as Text
import qualified Empire.API.Data.Node     as N
import qualified Empire.API.Data.NodeMeta as NM
import           Object.UITypes
import           Utils.PreludePlus
import           Utils.Vector

import           Data.Aeson               (ToJSON)
import           Event.Mouse              (MouseButton (..))
import           Object.Widget
import           Utils.CtxDynamic

data Node = Node { _nodeId     :: Int
                 , _controls   :: [Maybe WidgetId]
                 , _ports      :: [WidgetId]
                 , _position   :: Position
                 , _zPos       :: Double
                 , _expression :: Text
                 , _name       :: Text
                 , _value      :: Text
                 , _isExpanded :: Bool
                 , _isSelected :: Bool
                 , _isFocused  :: Bool
                 } deriving (Eq, Show, Typeable, Generic)

makeLenses ''Node
instance ToJSON Node

fromNode :: N.Node -> Node
fromNode n = case n ^. N.nodeType of
    N.ExpressionNode expression -> Node (n ^. N.nodeId) [] [] (uncurry Vector2 $ n ^. N.nodeMeta ^. NM.position) 0.0 expression (n ^. N.name) "" False False False
    N.InputNode inputIx         -> Node (n ^. N.nodeId) [] [] (uncurry Vector2 $ n ^. N.nodeMeta ^. NM.position) 0.0 (Text.pack $ "Input " <> show inputIx) (n ^. N.name) "" False False False
    N.OutputNode                -> Node (n ^. N.nodeId) [] [] (uncurry Vector2 $ n ^. N.nodeMeta ^. NM.position) 0.0 "Output"              (n ^. N.name) "" False False False
    N.ModuleNode                -> Node (n ^. N.nodeId) [] [] (uncurry Vector2 $ n ^. N.nodeMeta ^. NM.position) 0.0 "Module"              (n ^. N.name) "" False False False
    N.FunctionNode tpeSig       -> Node (n ^. N.nodeId) [] [] (uncurry Vector2 $ n ^. N.nodeMeta ^. NM.position) 0.0 "Function"            (n ^. N.name) (Text.pack $ intercalate " -> " tpeSig) False False False

instance IsDisplayObject Node where
    widgetPosition = position
    widgetSize     = lens get set where
        get _      = Vector2 60.0 60.0
        set w _    = w
    widgetVisible  = to $ const True

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
    widgetVisible  = to $ const True
