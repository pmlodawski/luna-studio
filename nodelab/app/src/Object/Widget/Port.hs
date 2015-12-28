module Object.Widget.Port where

import Utils.PreludePlus
import Utils.Vector
import Object.UITypes
import Utils.Angle (toAngle)
import Object.Widget
import Object.Node
import Data.Aeson (ToJSON)

import qualified JS.Widget as UI

data Port = Port { _portRef     :: PortRef
                 , _angleVector :: Vector2 Double
                 , _color       :: Int
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


-- instance HandlesMouseOver Port where
--     onMouseOver file model = (action, toCtxDynamic model) where
--                   action   = UI.setWidgetFocused (file ^. objectId) True
--
-- instance HandlesMouseOut Port where
--     onMouseOut  file model = (action, toCtxDynamic model) where
--                   action   = UI.setWidgetFocused (file ^. objectId) False
--
