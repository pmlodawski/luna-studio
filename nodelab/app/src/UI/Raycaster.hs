module UI.Raycaster where

import           Utils.PreludePlus

import           Utils.Vector

import           GHCJS.Marshal     (fromJSVal)
import           GHCJS.Marshal.Pure (pFromJSVal)
import           GHCJS.Types       (JSVal)
import           JavaScript.Array  (JSArray)
import qualified JavaScript.Array  as JSArray

import           Object.Widget (WidgetId, SceneType(..))
import           Event.Event (JSState)

foreign import javascript safe "raycaster.getMapPixelAt($1, $2)" getMapPixelAtJS :: Int -> Int -> IO JSArray

getMapPixelAt :: Vector2 Int -> IO JSArray
getMapPixelAt pos = getMapPixelAtJS (pos ^. x) (pos ^. y)

foreign import javascript safe "raycaster.getObjectsInRect($2, $3, $4, $5)" getObjectsInRect' :: JSState -> Int -> Int -> Int -> Int -> JSArray

getObjectsInRect :: JSState -> Vector2 Int -> Vector2 Int -> [Int]
getObjectsInRect jsstate pos size = list where
    idsJS = getObjectsInRect' jsstate (pos ^. x) (pos ^. y) (size ^. x) (size ^. y)
    list  = (pFromJSVal :: JSVal -> Int) <$> JSArray.toList idsJS

foreign import javascript safe "raycaster.widgetMatrix($1)" widgetMatrix :: Int -> IO JSArray

readObjectId :: Vector2 Int -> IO (Maybe WidgetId)
readObjectId pos = do
    pixel <- getMapPixelAt pos
    let read i = fromJSVal $ JSArray.index i pixel :: IO (Maybe Int)
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
    let read i = fromJSVal $ JSArray.index i worldMatrix :: IO (Maybe Double)
    elems <- mapM read [0..15]
    return $ Just $ catMaybes elems
readWidgetMatrix  Nothing   = return Nothing

foreign import javascript safe "raycaster.isWorkspace($1)" whichSceneJS :: Int -> IO Bool

whichScene :: Maybe WidgetId -> IO (Maybe SceneType)
whichScene (Just oid) = do
    sceneType <- whichSceneJS oid
    return $ Just $ if sceneType then Workspace else HUD
whichScene Nothing = return Nothing


