{-# LANGUAGE ExistentialQuantification #-}

module Object.Widget.Node where

import           Utils.PreludePlus
import           Utils.Vector
import           Object.UITypes
import           Data.Fixed

import           Object.Widget
import           Utils.CtxDynamic

import qualified JS.Node as UI

data Node = Node { _nodeId    :: Int
                 , _controls  :: [WidgetId]
                 , _ports     :: [WidgetId]
                 } deriving (Eq, Show, Typeable)

makeLenses ''Node

data PendingNode = PendingNode { _position   :: Vector2 Double
                               , _expression:: Text
                               } deriving (Eq, Show, Typeable)

makeLenses ''PendingNode

instance IsDisplayObject Node where
    objectPosition   = undefined
    objectSize       = undefined

instance IsDisplayObject PendingNode where
    objectPosition n = n ^. position
    objectSize _     = Vector2 60.0 60.0

instance Focusable Node where
    mayFocus LeftButton _ _ _ = True
    mayFocus _          _ _ _ = False

instance HandlesKeyPressed Node where
    onKeyPressed char _ file model = (action, toCtxDynamic model) where
        action = case char of
            '\r' -> do
                node <- UI.getNode (model ^. nodeId)
                UI.toggleExpandState node
            _    -> return ()
