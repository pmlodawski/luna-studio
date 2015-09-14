{-# LANGUAGE ExistentialQuantification #-}

module Object.Widget.Port where

import           Utils.PreludePlus
import           Utils.Vector
import           Object.UITypes
import           Data.Fixed

import           Object.Widget
import           Object.Node
import           Utils.CtxDynamic

import JS.Bindings

data Port = Port { _portRef :: PortRef
                 } deriving (Eq, Show, Typeable)

makeLenses ''Port

instance IsDisplayObject Port where
    objectPosition   = undefined
    objectSize       = undefined
