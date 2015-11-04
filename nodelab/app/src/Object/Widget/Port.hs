{-# LANGUAGE ExistentialQuantification #-}

module Object.Widget.Port where

import           Utils.PreludePlus
import           Utils.Vector
import           Object.UITypes
import           Data.Fixed
import           Utils.Angle (toAngle)

import           Object.Widget
import           Object.Node
import           Utils.CtxDynamic

import qualified JS.Widget as UI

data Port = Port { _portRef     :: PortRef
                 , _angleVector :: Vector2 Double
                 , _color       :: Int
                 } deriving (Eq, Show, Typeable)

makeLenses ''Port

angle :: Getter Port Double
angle = to (toAngle . view angleVector )

instance IsDisplayObject Port where
    widgetPosition = lens (\x -> Vector2 0.0 0.0) (error "Port has no position setter")


instance HandlesMouseOver Port where
    onMouseOver file model = (action, toCtxDynamic model) where
                  action   = UI.setWidgetFocused (file ^. objectId) True

instance HandlesMouseOut Port where
    onMouseOut  file model = (action, toCtxDynamic model) where
                  action   = UI.setWidgetFocused (file ^. objectId) False

