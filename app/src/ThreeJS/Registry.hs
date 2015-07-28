{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE JavaScriptFFI #-}

module ThreeJS.Registry where

import           Utils.PreludePlus

import           Data.Text.Lazy      ( Text )

import           GHCJS.Foreign
import           GHCJS.Types         ( JSRef, JSString )
import           JavaScript.Array    ( JSArray )
import qualified JavaScript.Array   as JSArray
import           Data.JSString.Text  ( lazyTextToJSString )



foreign import javascript unsafe "$$.buttons[$1]"
    getFromRegistryJS :: Int -> IO (JSRef a)

foreign import javascript unsafe "$$.buttons[$1] = $2"
    putToRegistryJS :: Int -> JSRef a -> IO ()

foreign import javascript unsafe "delete $$.buttons[$1]"
    removeFromRegistryJS :: Int -> IO ()


-- class Widget a b where
--     getFromRegistry    :: a -> IO b
--     putToRegistry      :: a -> b -> IO ()
--     removeFromRegistry :: a -> IO ()