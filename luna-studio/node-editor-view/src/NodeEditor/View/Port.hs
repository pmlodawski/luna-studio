module NodeEditor.View.Port where

import           Common.Data.JSON            (toJSONVal)
import           Common.Prelude
import           Data.Aeson                  (ToJSON)
import           Data.Convert                (Convertible (convert))
import           NodeEditor.React.Model.Port (InPort, OutPort)
import qualified NodeEditor.React.Model.Port as Port



data PortView = PortView
        { key :: String
        } deriving (Generic, Show)

instance ToJSON PortView
instance Convertible InPort PortView where
    convert c = PortView
        {- key        -} (c ^. Port.portId . to show)

instance Convertible OutPort PortView where
    convert c = PortView
        {- key        -} (c ^. Port.portId . to show)
        