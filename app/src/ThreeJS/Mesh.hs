module ThreeJS.Mesh where

import           Utils.PreludePlus

import           GHCJS.Foreign
import           GHCJS.Types      ( JSRef, JSString )
import           ThreeJS.Types

foreign import javascript unsafe "new THREE.Mesh($1, $2)"
    buildMeshJS :: JSRef a -> JSRef b -> IO Mesh

buildMesh ::  (Geometry a, Material b) => JSRef a -> JSRef b -> IO Mesh
buildMesh = buildMeshJS

foreign import javascript unsafe "new THREE.Group()"
    buildGroupJS :: IO Mesh

-- data Group = Group Mesh

buildGroup :: IO Group
buildGroup = buildGroupJS >>= return . Group

foreign import javascript unsafe "$1.add($2)" addJS :: JSRef a -> JSRef b -> IO ()
foreign import javascript unsafe "$1.remove($2)" removeJS :: JSRef a -> JSRef b -> IO ()

instance Container Group where
    add    (Group m) o = m `addJS`    (mesh o)
    remove (Group m) o = m `removeJS` (mesh o)


foreign import javascript unsafe "$1.position"
    position :: Mesh -> IO (JSRef JSVector2)

foreign import javascript unsafe "$1.scale"
    scale :: Mesh -> IO (JSRef JSVector2)

