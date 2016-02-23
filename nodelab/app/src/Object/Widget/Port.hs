module Object.Widget.Port where

import           Data.Aeson               (ToJSON)
import           Utils.Angle              (toAngle)
import           Utils.PreludePlus
import           Utils.Vector

import           Empire.API.Data.PortRef  (AnyPortRef)
import qualified Empire.API.JSONInstances ()

import           Object.Node
import           Object.UITypes
import           Object.Widget

import qualified JS.Widget                as UI

data Port = Port { _portRef     :: AnyPortRef
                 , _angleVector :: Vector2 Double
                 , _portCount   :: Int
                 , _color       :: Int
                 , _highlight   :: Bool
                 } deriving (Eq, Show, Typeable, Generic)

makeLenses ''Port
instance ToJSON Port

angle :: Getter Port Double
angle = to (toAngle . view angleVector )

instance IsDisplayObject Port where
    widgetPosition = lens (\x -> Vector2 0.0 0.0) (error "Port has no position setter")
    widgetSize     = lens get set where
        get _      = Vector2 0.0 0.0
        set w _    = w
    widgetVisible  = to $ const True
