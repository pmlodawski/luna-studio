module ThreeJS.Scene where

-- import           Utils.PreludePlus
--
-- import           GHCJS.Foreign
-- import           GHCJS.Types      ( JSRef, JSString )
-- import           ThreeJS.Types
-- import           ThreeJS.Mesh
--
--
-- instance Container Scene where
--     add    (Scene m) o = mesh o >>= addJS m
--     remove (Scene m) o = mesh o >>= removeJS m
--
-- foreign import javascript unsafe "$$.scene" sceneJS :: JSRef Scene
-- scene = Scene sceneJS
--
-- foreign import javascript unsafe "$$.sceneHUD" sceneHUDJS :: JSRef Scene
-- sceneHUD = Scene sceneHUDJS
--
