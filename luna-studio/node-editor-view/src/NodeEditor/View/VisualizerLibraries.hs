module NodeEditor.View.VisualizerLibraries where

import Common.Prelude

import qualified Control.Lens.Aeson                   as Lens
import qualified Data.Map                             as Map
import qualified NodeEditor.React.Model.Visualization as Visualization

import Common.Data.JSON                     (toJSONVal)
import Data.Aeson                           (ToJSON (toEncoding, toJSON))
import Data.Convert                         (Convertible (convert))
import Data.Map                             (Map)
import NodeEditor.React.Model.Visualization (Visualizers)
import NodeEditor.View.Diff                 (DiffT, diffApply)


data VisualizerLibrariesView = VisualizerLibrariesView
    { _internalVisualizersPath  :: String
    , _lunaVisualizersPath      :: String
    , _projectVisualizersPath   :: Maybe String
    , _importedVisualizersPaths :: Map String String
    } deriving (Eq, Generic, Show)

makeLenses ''VisualizerLibrariesView

instance ToJSON VisualizerLibrariesView where
    toEncoding = Lens.toEncoding
    toJSON     = Lens.toJSON

instance Convertible (Visualizers FilePath) VisualizerLibrariesView where
    convert vp = VisualizerLibrariesView
        {- internalVisualizersPath  -} (vp ^. Visualization.internalVisualizers)
        {- lunaVisualizersPath      -} (vp ^. Visualization.lunaVisualizers)
        {- projectVisualizersPath   -} (vp
            ^. Visualization.externalVisualizers
            .  Visualization.projectVisualizers)
        {- importedVisualizersPaths -} (Map.mapKeys convert $ vp
            ^. Visualization.externalVisualizers
            .  Visualization.librariesVisualizers)

foreign import javascript safe
    "callback.getNodeEditorView().setVisualizerLibraries($1)"
    setVisualizerLibraries__ :: JSVal -> IO ()

setVisualizerLibraries :: MonadIO m => VisualizerLibrariesView -> m ()
setVisualizerLibraries = liftIO . setVisualizerLibraries__ <=< toJSONVal

visualizerLibrariesView :: MonadIO m => DiffT (Visualizers FilePath) m ()
visualizerLibrariesView = diffApply $ setVisualizerLibraries . convert
