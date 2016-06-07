{-# LANGUAGE OverloadedStrings #-}
module JS.Cursor
    ( setCursor
    , Cursor(..)
    ) where

import           Data.JSString     ()
import           GHCJS.Types       (JSString)
import           Utils.PreludePlus

foreign import javascript safe "$('#canvas2d').css({cursor: $1})" setCursor' :: JSString -> IO ()

data Cursor = Normal | Pointer deriving (Show);

setCursor :: Cursor -> IO ()
setCursor Normal  = setCursor' "default"
setCursor Pointer = setCursor' "Pointer"
