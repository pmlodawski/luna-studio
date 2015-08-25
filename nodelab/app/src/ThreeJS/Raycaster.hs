module ThreeJS.Raycaster where

import           Utils.PreludePlus

import           GHCJS.Foreign
import           GHCJS.Types      ( JSRef, JSString )
import           Utils.Vector
import qualified Event.Mouse         as Mouse
import           JavaScript.Array    (JSArray)
import qualified JavaScript.Array    as JSArray
import           GHCJS.Marshal
import           ThreeJS.Types
import           Object.UITypes



foreign import javascript unsafe "raycaster.getMapPixelAt($1, $2)"
    getMapPixelAtJS :: Int -> Int -> IO JSArray

getMapPixelAt :: Vector2 Int -> IO JSArray
getMapPixelAt pos = getMapPixelAtJS (pos ^. x) (pos ^. y)

foreign import javascript unsafe "raycaster.widgetMatrix($1)"
    widgetMatrix :: Int -> IO JSArray


readObjectId :: Vector2 Int -> IO (Maybe WidgetId)
readObjectId pos = do
    pixel <- getMapPixelAt pos
    let read i = fromJSRef $ JSArray.index i pixel :: IO (Maybe Int)
    maybeR <- read 0
    maybeG <- read 1
    maybeB <- read 2
    -- maybeA <- read 3
    return $ do
        r <- maybeR
        g <- maybeG
        b <- maybeB
        let oid = r + 256 * g + 256 * 256 * b
        if oid == 0 then Nothing
                    else Just oid

readWidgetMatrix :: Maybe WidgetId -> IO (Maybe [Double])
readWidgetMatrix (Just oid) = do
    worldMatrix <- widgetMatrix oid
    let read i = fromJSRef $ JSArray.index i worldMatrix :: IO (Maybe Double)
    elems <- mapM read [0..15]
    return $ Just $ catMaybes elems
readWidgetMatrix  Nothing   = return Nothing

foreign import javascript unsafe "raycaster.isWorkspace($1)"
    whichSceneJS :: Int -> IO Bool

whichScene :: Maybe WidgetId -> IO (Maybe SceneType)
whichScene (Just oid) = do
    sceneType <- whichSceneJS oid
    return $ Just $ if sceneType then Workspace else HUD
whichScene Nothing = return Nothing


