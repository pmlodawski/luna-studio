module ThreeJS.PlaneGeometry where

import           Utils.PreludePlus

import           GHCJS.Foreign
import           GHCJS.Types      ( JSRef, JSString )
import           ThreeJS.Types

data PlaneGeometry

instance Geometry PlaneGeometry


foreign import javascript unsafe "new THREE.PlaneBufferGeometry($1, $2)"
    buildPlaneGeometry :: Double -> Double -> IO (JSRef PlaneGeometry)