module NodeEditor.View.Port where

import           Common.Prelude
import qualified Control.Lens.Aeson          as Lens
import           Data.Aeson                  (FromJSON, ToJSON (toEncoding, toJSON))
import           Data.Convert                (Convertible (convert))
import           NodeEditor.React.Model.Port (InPort, OutPort)
import qualified NodeEditor.React.Model.Port as Port
import           NodeEditor.View.Color       (RGB)


data PortView = PortView
        { _key   :: String
        , _color :: RGB
        , _name  :: String
        , _mode  :: String
        } deriving (Eq, Generic, Show)

makeLenses ''PortView

instance FromJSON PortView
instance NFData   PortView
instance ToJSON   PortView where
    toEncoding = Lens.toEncoding
    toJSON     = Lens.toJSON

instance Convertible InPort PortView where
    convert c = PortView
        {- key   -} (c ^. Port.portId . to show)
        {- color -} (c ^. Port.color . to convert)
        {- name  -} (c ^. Port.name . to convert)
        {- mode  -} (if c ^. Port.portId == [Port.Self] then "self" else "in")

instance Convertible OutPort PortView where
    convert c = PortView
        {- key   -} (c ^. Port.portId . to show)
        {- color -} (c ^. Port.color . to convert)
        {- name  -} (c ^. Port.name . to convert)
        {- mode  -} def
        