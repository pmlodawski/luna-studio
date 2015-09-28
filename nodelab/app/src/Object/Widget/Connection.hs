{-# LANGUAGE ExistentialQuantification #-}

module Object.Widget.Connection where

import           Utils.PreludePlus
import           Utils.Vector
import           Object.UITypes
import           Data.Fixed

import           Object.Object
import           Object.Widget
import           Object.Node

data Connection = Connection { _connectionId :: ConnectionId
                             , _visible      :: Bool
                             , _from         :: Vector2 Double
                             , _to           :: Vector2 Double
                             } deriving (Eq, Show, Typeable)

makeLenses ''Connection

instance IsDisplayObject Connection where
    objectPosition   = undefined
    objectSize       = undefined



data CurrentConnection = CurrentConnection deriving (Eq, Show, Typeable)

makeLenses ''CurrentConnection

instance IsDisplayObject CurrentConnection where
    objectPosition   = undefined
    objectSize       = undefined
