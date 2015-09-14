{-# LANGUAGE ExistentialQuantification #-}

module Object.Widget.Node where

import           Utils.PreludePlus
import           Utils.Vector
import           Object.UITypes
import           Data.Fixed

import           Object.Widget
import           Utils.CtxDynamic

import JS.Bindings

data Node = Node { _nodeId    :: Int
                 , _controls  :: [WidgetId]
                 } deriving (Eq, Show, Typeable)

makeLenses ''Node

instance IsDisplayObject Node where
    objectPosition   = undefined
    objectSize       = undefined


instance DblClickable Node where
    onDblClick pos file model = (action, toCtxDynamic model) where
        action    = do
            node <- getNode (model ^. nodeId)
            toggleExpandState node
            putStrLn $ "Clicked node: " <> show (model ^. nodeId)
