module JS.Visualizers where

import           Common.Prelude             hiding (toList)
import           Control.Arrow              ((&&&))
import qualified Data.Aeson                 as Aeson
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import           Data.UUID.Types            (UUID)
import           GHCJS.Marshal              (toJSValListOf)
import           GHCJS.Marshal.Pure         (pFromJSVal)
import           IdentityString             (IdentityString)
import qualified IdentityString             as IS
import           JavaScript.Array           (JSArray, toList)
import           LunaStudio.Data.TypeRep    (ConstructorRep)


foreign import javascript safe "window.getInternalVisualizersPath()"
    getInternalVisualizersLibraryPath' :: IO JSString

getInternalVisualizersLibraryPath :: MonadIO m => m FilePath
getInternalVisualizersLibraryPath
    = liftIO $ convert <$> getInternalVisualizersLibraryPath'

foreign import javascript safe "window.getLunaVisualizersPath()"
    getLunaVisualizersLibraryPath' :: IO JSString

getLunaVisualizersLibraryPath :: MonadIO m => m FilePath
getLunaVisualizersLibraryPath
    = liftIO $ convert <$> getLunaVisualizersLibraryPath'

foreign import javascript safe "res = window.getInternalVisualizers(); $r = Object.keys(typeof res == 'object' ? res : {});"
    getInternalVisualizers' :: IO JSArray

getInternalVisualizers :: MonadIO m => m [String]
getInternalVisualizers
    = liftIO $ fmap pFromJSVal . toList <$> getInternalVisualizers'

foreign import javascript safe "res = window.getLunaVisualizers(); $r = Object.keys(typeof res == 'object' ? res : {});"
    getLunaVisualizers' :: IO JSArray

getLunaVisualizers :: MonadIO m => m [String]
getLunaVisualizers
    = liftIO $ fmap pFromJSVal . toList <$> getLunaVisualizers'

foreign import javascript safe "res = window.getProjectVisualizers($1); $r = Object.keys(typeof res == 'object' ? res : {});"
    getProjectVisualizers' :: JSString -> IO JSArray

getProjectVisualizers :: MonadIO m => FilePath -> m [String]
getProjectVisualizers fp
    = liftIO $ fmap pFromJSVal . toList <$> getProjectVisualizers' (convert fp)

foreign import javascript safe "window.checkInternalVisualizer($1)"
    checkInternalVisualizer' :: JSString -> IO JSString

checkInternalVisualizer :: MonadIO m => String -> m String
checkInternalVisualizer name
    = liftIO $ convert <$> checkInternalVisualizer' (convert name)

foreign import javascript safe "window.checkLunaVisualizer($1, $2)"
    checkLunaVisualizer' :: JSString -> JSString -> IO JSString

checkLunaVisualizer :: MonadIO m => String -> String -> m String
checkLunaVisualizer name rep
    = liftIO $  convert <$> checkLunaVisualizer' (convert name) (convert rep)

foreign import javascript safe "window.checkProjectVisualizer($1, $2)"
    checkProjectVisualizer' :: JSString -> JSString -> IO JSString

checkProjectVisualizer :: MonadIO m => String -> String -> m String
checkProjectVisualizer name rep
    = liftIO $ convert <$> checkProjectVisualizer' (convert name) (convert rep)

mkInternalVisualizersMap :: MonadIO m => m (Map String String)
mkInternalVisualizersMap = liftIO $ getInternalVisualizers >>= fmap Map.fromList
    . mapM (\name -> (name,) <$> checkInternalVisualizer name)

mkLunaVisualizersMap :: MonadIO m => m (Map String (String -> m String))
mkLunaVisualizersMap = liftIO
    $ Map.fromList . fmap (id &&& checkLunaVisualizer) <$> getLunaVisualizers

mkProjectVisualizersMap :: MonadIO m
    => FilePath -> m (Map String (String -> IO String))
mkProjectVisualizersMap fp = liftIO $ Map.fromList
    . fmap (id &&& checkProjectVisualizer) <$> getProjectVisualizers fp


foreign import javascript safe "visualizerFramesManager.sendData($1, $2, $3);"
    sendVisualizationData' :: JSString -> JSString -> JSString -> IO ()

foreign import javascript safe "visualizerFramesManager.sendInternalData($1, $2);"
    sendInternalData' :: JSString -> JSString -> IO ()

foreign import javascript safe "visualizerFramesManager.sendDatapoint($1, $2);"
    sendStreamDatapoint' :: JSString -> JSString -> IO ()

foreign import javascript safe "visualizerFramesManager.register($1);"
    registerVisualizerFrame' :: JSString -> IO ()

foreign import javascript safe "visualizerFramesManager.notifyStreamRestart($1, $2, $3)"
    notifyStreamRestart' :: JSString -> JSString -> JSVal -> IO ()

notifyStreamRestart :: MonadIO m => UUID -> ConstructorRep -> [IdentityString] -> m ()
notifyStreamRestart uid rep backup = liftIO $ notifyStreamRestart'
    (convert $ show uid)
    (convert . BS.unpack $ Aeson.encode rep)
    =<< toJSValListOf (view IS.jsString <$> backup)

sendStreamDatapoint :: MonadIO m => UUID -> IdentityString -> m ()
sendStreamDatapoint uid d = liftIO $ sendStreamDatapoint'
    (convert $ show uid)
    (d ^. IS.jsString)

registerVisualizerFrame :: MonadIO m => UUID -> m ()
registerVisualizerFrame uuid
    = liftIO $ registerVisualizerFrame' (convert $ show uuid)

sendVisualizationData :: MonadIO m => UUID -> ConstructorRep -> IdentityString -> m ()
sendVisualizationData uid rep d = liftIO $ sendVisualizationData'
    (convert $ show uid)
    (convert . BS.unpack $ Aeson.encode rep)
    (d ^. IS.jsString)

sendInternalData :: MonadIO m => UUID -> Text -> m ()
sendInternalData uid d = liftIO $ sendInternalData'
    (convert $ show uid)
    (convert d)
