{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}

module UI.Registry where

import           Utils.PreludePlus

import           GHCJS.Foreign
import           GHCJS.Types         (JSVal)
import           Object.Widget
import qualified Data.JSString as JSString
import           Object.UITypes
import           GHCJS.Marshal.Pure(PToJSVal(..), PFromJSVal(..))
import           UI.Types(UIWidget(..))

foreign import javascript unsafe "common.registry[$1]"
    lookup' :: Int -> IO JSVal

foreign import javascript unsafe "common.registry[$1] = $2"
    register' :: Int -> JSVal -> IO ()

foreign import javascript unsafe "delete common.registry[$1]"
    unregister' :: Int -> IO ()

lookup :: UIWidget b => WidgetId -> IO (b)
lookup oid = lookup' oid >>= return . pFromJSVal

register :: UIWidget b => WidgetId -> b -> IO ()
register oid widget = register' oid (pToJSVal widget)

unregister :: WidgetId -> IO ()
unregister = unregister'
