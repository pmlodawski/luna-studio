{-# LANGUAGE ExistentialQuantification #-}

module Object.Widget.Connection where

import           Utils.PreludePlus
import           Utils.Vector
import           Object.UITypes
import           Data.Fixed

import           Object.Object
import           Object.Widget
import           Object.Node

import qualified JS.Widget as UI
import           Utils.CtxDynamic

data Connection = Connection { _connectionId :: ConnectionId
                             , _visible      :: Bool
                             , _from         :: Vector2 Double
                             , _to           :: Vector2 Double
                             } deriving (Eq, Show, Typeable)

makeLenses ''Connection

instance IsDisplayObject Connection

data CurrentConnection = CurrentConnection deriving (Eq, Show, Typeable)

makeLenses ''CurrentConnection

instance IsDisplayObject CurrentConnection

instance HandlesMouseOver Connection where
    onMouseOver file model = (action, toCtxDynamic model) where
                  action   = UI.setWidgetFocused (file ^. objectId) True

instance HandlesMouseOut Connection where
    onMouseOut  file model = (action, toCtxDynamic model) where
                  action   = UI.setWidgetFocused (file ^. objectId) False
