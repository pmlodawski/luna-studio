{-# LANGUAGE ExistentialQuantification #-}

module Object.Widget.Node where

import           Utils.PreludePlus
import           Utils.Vector
import           Object.UITypes
import           Data.Fixed

import           Object.Widget
import           Utils.CtxDynamic

import JS.Bindings

data Node = Node { _refId     :: Int
                 , _nodeId    :: Int
                 , _controls  :: [WidgetId]
                 } deriving (Eq, Show, Typeable)

makeLenses ''Node

instance IsDisplayObject Node where
    objectId       b = b ^. refId
    objectIdLens     = refId
    objectPosition   = undefined
    objectSize       = undefined


instance DblClickable Node where
    onDblClick pos n = (Just action, toCtxDynamic n) where
        action    = do
            node <- getNode (n ^. nodeId)
            toggleExpandState node
            putStrLn $ "Clicked node: " <> show (n ^. refId) <> " / " <> show (n ^. nodeId)
