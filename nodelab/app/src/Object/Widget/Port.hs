{-# LANGUAGE ExistentialQuantification #-}

module Object.Widget.Port where

import           Utils.PreludePlus
import           Utils.Vector
import           Object.UITypes
import           Data.Fixed

import           Object.Widget
import           Object.Node
import           Utils.CtxDynamic

import qualified JS.Widget as UI

data Port = Port { _portRef :: PortRef
                 } deriving (Eq, Show, Typeable)

makeLenses ''Port

instance IsDisplayObject Port where
    objectPosition   = undefined
    objectSize       = undefined


instance HandlesMouseOver Port where
    onMouseOver file model = (action, toCtxDynamic model) where
                  action   = UI.setWidgetFocused (file ^. objectId) True

instance HandlesMouseOut Port where
    onMouseOut  file model = (action, toCtxDynamic model) where
                  action   = UI.setWidgetFocused (file ^. objectId) False
