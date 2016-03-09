{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}

module Object.Widget.Node where

import           Data.Aeson                (ToJSON)
import           Data.Fixed
import           Utils.CtxDynamic
import           Utils.PreludePlus
import           Utils.Vector

import qualified Data.Text.Lazy            as Text
import qualified Empire.API.Data.Node      as N
import qualified Empire.API.Data.NodeMeta  as NM
import qualified Empire.API.Data.Port      as P
import qualified Empire.API.Data.ValueType as VT
import           Event.Mouse               (MouseButton (..))
import           Object.UITypes
import           Object.Widget

data Node = Node { _nodeId     :: Int
                 , _controls   :: [Maybe WidgetId]
                 , _ports      :: [WidgetId]
                 , _position   :: Position
                 , _zPos       :: Double
                 , _expression :: Text
                 , _name       :: Text
                 , _value      :: Text
                 , _tpe        :: Maybe Text
                 , _isExpanded :: Bool
                 , _isSelected :: Bool
                 , _isFocused  :: Bool
                 , _isError    :: Bool
                 , _execTime   :: Maybe Integer
                 } deriving (Eq, Show, Typeable, Generic)

makeLenses ''Node
instance ToJSON Node

makeNode :: Int -> Position -> Text -> Text -> Maybe Text -> Node
makeNode id pos expr name tpe = Node id [] [] pos 0.0 expr name "" tpe False False False False Nothing

fromNode :: N.Node -> Node
fromNode n = let position' = uncurry Vector2 $ n ^. N.nodeMeta ^. NM.position
                 nodeId'   = n ^. N.nodeId
                 name'     = n ^. N.name
    in
    case n ^. N.nodeType of
        N.ExpressionNode expression ->  makeNode nodeId' position' expression name' Nothing
        N.InputNode inputIx         ->  makeNode nodeId' position' (Text.pack $ "Input " <> show inputIx) name' (Just tpe) where
            tpe = Text.pack $ fromMaybe "?" $ show <$> n ^? N.ports . ix (P.OutPortId P.All) . P.valueType
        N.OutputNode outputIx       ->  makeNode nodeId' position' (Text.pack $ "Output " <> show outputIx) name' Nothing
        N.ModuleNode                ->  makeNode nodeId' position' "Module"    name' Nothing
        N.FunctionNode tpeSig       -> (makeNode nodeId' position' "Function"  name' Nothing) & value .~ (Text.pack $ intercalate " -> " tpeSig)


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
