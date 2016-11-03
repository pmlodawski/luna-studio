{-# LANGUAGE OverloadedStrings #-}
module Object.Widget.FunctionPort where

import           Data.Aeson                          (ToJSON)
import           Empire.API.Data.Input               (Input)
import qualified Empire.API.Data.Input               as Input
import           Empire.API.Data.Node                (NodeId)
import           Empire.API.Data.Output              (Output)
import qualified Empire.API.Data.Output              as Output
import           Empire.API.Data.Port                (PortId (InPortId, OutPortId))
import           Empire.API.Data.PortRef             (AnyPortRef, toAnyPortRef)
import qualified Empire.API.Data.ValueType           as ValueType
import           Object.Widget
import qualified Reactive.Commands.Node.Ports.Colors as Colors
import           Utils.PreludePlus
import           Utils.Vector



data InputOutput = Input | Output
        deriving (Eq, Show, Typeable, Generic)


data FunctionPort = FunctionPort
                    { _position          :: Vector2 Double
                    , _hovered           :: Bool
                    , _size              :: Vector2 Double
                    , _inputOutput       :: InputOutput
                    , _portRef           :: AnyPortRef
                    , _color             :: Int
                    , _unhoverLabelValue :: Text
                    , _hoverLabelValue   :: Text
                    } deriving (Eq, Show, Typeable, Generic)

makeLenses ''FunctionPort

instance ToJSON InputOutput
instance ToJSON FunctionPort
instance IsDisplayObject FunctionPort where
    widgetPosition = position
    widgetSize     = size
    widgetVisible  = to $ const True

labelValue :: Getter FunctionPort Text
labelValue = to $ \fp -> if fp ^. hovered
    then fp ^. hoverLabelValue
    else fp ^. unhoverLabelValue

create :: InputOutput -> AnyPortRef -> Int -> Text -> Text -> FunctionPort
create = FunctionPort def False (Vector2 50 50)

fromInput :: NodeId -> Input  -> FunctionPort
fromInput nodeId input = create Input port (Colors.vtToColor vt) label labelWithType where
    port  = toAnyPortRef nodeId $ OutPortId $ input ^. Input.outPort
    vt    = input ^. Input.valueType
    label = input ^. Input.name
    labelWithType = label <> " :: " <> vt ^. ValueType.toText

fromOutput :: NodeId -> Output -> FunctionPort
fromOutput nodeId output = create Output port (Colors.vtToColor vt) label label where
    port  = toAnyPortRef nodeId $ InPortId $ output ^. Output.inPort
    vt    = output ^. Output.valueType
    label = "-> " <> vt ^. ValueType.toText
