{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE JavaScriptFFI #-}

module JS.Node where

import           Utils.PreludePlus
import           Utils.Vector

import           Data.Dynamic
import           Data.Text.Lazy      (Text)

import           GHCJS.Foreign
import           GHCJS.DOM.EventM
import           GHCJS.DOM           (currentDocument)
import           GHCJS.DOM.Element   (Element, IsElement)
import           GHCJS.Types         (JSVal, JSString)
import           GHCJS.DOM.Types     (UIEvent, IsUIEvent, unUIEvent, toUIEvent)
import           JavaScript.Array    (JSArray)
import qualified JavaScript.Array    as JSArray
import           Data.JSString.Text  (lazyTextToJSString)
import           Data.JSString       (unpack)

foreign import javascript unsafe "app.createPendingNode($1, $2, $3, $4)"
    createPendingNode' :: Int -> JSString -> Double -> Double -> IO ()

createPendingNode :: Int -> Text -> Vector2 Double -> IO ()
createPendingNode oid expr (Vector2 x y) = createPendingNode' oid (lazyTextToJSString expr) x y

