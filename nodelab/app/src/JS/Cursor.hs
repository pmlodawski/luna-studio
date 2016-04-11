{-# LANGUAGE JavaScriptFFI     #-}
{-# LANGUAGE OverloadedStrings #-}

module JS.Cursor where

import qualified Data.JSString     as JSString
import           GHCJS.Foreign
import           GHCJS.Types       (JSString)
import           Utils.PreludePlus
import           Utils.Vector

foreign import javascript safe "$('#canvas2d').css({cursor: $1})" setCursor' :: JSString -> IO ()

data Cursor = Normal | Pointer deriving (Show);

setCursor :: Cursor -> IO ()
setCursor Normal  = setCursor' "default"
setCursor Pointer = setCursor' "Pointer"