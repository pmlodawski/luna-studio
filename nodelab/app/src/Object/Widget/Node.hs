{-# LANGUAGE ExistentialQuantification #-}

module Object.Widget.Node where

import           Utils.PreludePlus
import           Utils.Vector
import           Data.Fixed

import           Object.Widget
import           Utils.CtxDynamic


data Node = Node { _refId     :: Int
                 , _nodeId    :: Int
                 } deriving (Eq, Show, Typeable)

makeLenses ''Node

instance IsDisplayObject Node where
    objectId       b = b ^. refId
    objectIdLens     = refId
    objectPosition   = undefined
    objectSize       = undefined


instance Clickable Node where
    onClick pos n = (Just action, toCtxDynamic n) where
        action    = do
            putStrLn $ "Clicked node: " <> show (n ^. refId) <> " / " <> show (n ^. nodeId)
