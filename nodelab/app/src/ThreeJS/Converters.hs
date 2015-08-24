{-# LANGUAGE OverloadedStrings #-}

module ThreeJS.Converters where

import           Utils.PreludePlus
import           GHCJS.Foreign
import           GHCJS.Types      ( JSRef, JSString )
import           GHCJS.Prim



foreign import javascript unsafe "$r = typeof($1) === 'number' ? ($1|0) : 0;"
  js_fromJSDouble :: JSRef a -> Double

foreign import javascript unsafe "$r = $1;"
  js_toJSDouble :: Double -> JSRef a

fromJSDouble :: JSRef a -> Double
fromJSDouble = js_fromJSDouble
{-# INLINE fromJSDouble #-}

toJSDouble :: Double -> JSRef a
toJSDouble = js_toJSDouble
{-# INLINE toJSDouble #-}
