{-# LANGUAGE ExistentialQuantification #-}

module Object.Widget.Node where

import           Utils.PreludePlus
import           Utils.Vector
import           Data.Fixed

import           Object.Widget


data Number a =  Node   { _refId     :: Int
                        , _nodeId    :: Int
                        } deriving (Eq, Show, Typeable)

makeLenses ''Node

instance IsDisplayObject (Number a) where
    objectId       b = b ^. refId
    objectPosition   = undefined
    objectSize       = undefined
    objectIdLens     = undefined
