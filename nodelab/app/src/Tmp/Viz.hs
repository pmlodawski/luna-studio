{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE JavaScriptFFI #-}

module Tmp.Viz where

import           Utils.PreludePlus
import           Data.Text.Lazy      (Text)

import           GHCJS.Foreign
import           GHCJS.Types         (JSString)
import           Data.JSString.Text  (lazyTextToJSString)


foreign import javascript unsafe "window.open('data:image/svg+xml;base64,'+btoa(Viz($1, 'svg', 'dot')), 'graph')"
    displayGraphJS :: JSString -> IO ()

displayGraph :: Text -> IO ()
displayGraph = displayGraphJS . lazyTextToJSString
