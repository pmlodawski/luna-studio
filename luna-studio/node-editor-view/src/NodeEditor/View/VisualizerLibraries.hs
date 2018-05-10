module NodeEditor.View.VisualizerLibraries where

import           Common.Data.JSON                     (toJSONVal)
import           Common.Prelude
import qualified Control.Lens.Aeson                   as Lens
import           Data.Aeson                           (ToJSON (toEncoding, toJSON))
import           Data.Convert                         (Convertible (convert))
import           NodeEditor.React.Model.NodeEditor (VisualizersPaths)
import qualified NodeEditor.React.Model.NodeEditor as NE
import           NodeEditor.View.Diff                 (DiffT, diffApply)


data VisualizerLibrariesView = VisualizerLibrariesView
    { _internalVisualizersPath :: String
    , _lunaVisualizersPath     :: String
    , _projectVisualizersPath  :: Maybe String
    } deriving (Eq, Generic, Show)

makeLenses ''VisualizerLibrariesView

instance ToJSON VisualizerLibrariesView where
    toEncoding = Lens.toEncoding
    toJSON     = Lens.toJSON

instance Convertible VisualizersPaths VisualizerLibrariesView where
    convert vp = VisualizerLibrariesView
        {- internalVisualizersPath -} (vp ^. NE.internalVisualizersPath)
        {- lunaVisualizersPath     -} (vp ^. NE.lunaVisualizersPath)
        {- projectVisualizersPath  -} (vp ^. NE.projectVisualizersPath)

foreign import javascript safe
    "atomCallback.getNodeEditorView().setVisualizerLibraries($1)"
    setVisualizerLibraries__ :: JSVal -> IO ()

setVisualizerLibraries :: MonadIO m => VisualizerLibrariesView -> m ()
setVisualizerLibraries = liftIO . setVisualizerLibraries__ <=< toJSONVal

visualizerLibrariesView :: MonadIO m => DiffT VisualizersPaths m ()
visualizerLibrariesView = diffApply $ setVisualizerLibraries . convert
