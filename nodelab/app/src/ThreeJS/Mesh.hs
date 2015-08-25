module ThreeJS.Mesh where

import           Utils.PreludePlus

import           GHCJS.Foreign
import           GHCJS.Types      ( JSRef, JSString )
import           ThreeJS.Types
import           Utils.Vector

foreign import javascript unsafe "new THREE.Mesh($1, $2)"
    buildMeshJS :: JSRef a -> JSRef b -> IO Mesh

buildMesh :: (Geometry a, IsMaterial b) => JSRef a -> b -> IO Mesh
buildMesh g m = buildMeshJS g (material m)

foreign import javascript unsafe "new THREE.Group()"
    buildGroupJS :: IO Mesh

-- data Group = Group Mesh

buildGroup :: IO Group
buildGroup = buildGroupJS >>= return . Group

foreign import javascript unsafe "$1.add($2)"       addJS :: JSRef a -> JSRef b -> IO ()
foreign import javascript unsafe "$1.remove($2)" removeJS :: JSRef a -> JSRef b -> IO ()

instance Container Group where
    add    (Group m) o = mesh o >>= addJS m
    remove (Group m) o = mesh o >>= removeJS m


foreign import javascript unsafe "$1.position"
    position :: Mesh -> IO (JSRef JSVector2)

foreign import javascript unsafe "$1.scale"
    scale :: Mesh -> IO (JSRef JSVector2)


moveTo :: Vector2 Double -> Mesh -> IO ()
moveTo pos mesh = do
    p <- position mesh
    p `setX` (pos ^. x)
    p `setY` (pos ^. y)

moveBy :: Vector2 Double -> Mesh -> IO ()
moveBy pos mesh = do
    p <- position mesh
    p `addX` (pos ^. x)
    p `addY` (pos ^. y)


scaleBy :: Vector2 Double -> Mesh -> IO ()
scaleBy factor mesh = do
    s <- scale mesh
    s `setX` (factor ^. x)
    s `setY` (factor ^. y)

