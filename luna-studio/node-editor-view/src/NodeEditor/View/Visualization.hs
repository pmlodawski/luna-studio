module NodeEditor.View.Visualization where

import           Common.Data.JSON                     (toJSONVal)
import           Common.Prelude
import qualified Control.Lens.Aeson                   as Lens
import           Data.Aeson                           (ToJSON (toEncoding, toJSON))
import           Data.Convert                         (Convertible (convert))
import           Data.Map                             (Map)
import qualified Data.Map                             as Map
import           NodeEditor.React.Model.Visualization (NodeVisualizations, Visualization, VisualizationId, Mode,
                                                       Mode (Default, Focused, FullScreen, Preview, Hidden), Visualizer,
                                                       VisualizerType (InternalVisualizer, LunaVisualizer, ProjectVisualizer))
import qualified NodeEditor.React.Model.Visualization as Vis
import           NodeEditor.View.Diff                 (DiffT, diffApply, diffConvert, diffMap, diffMapWithKey)
import           NodeEditor.View.NodeLoc              (NodeLoc)

type VisualizerName = String

data VisualizerView = VisualizerView
    { _visualizerName :: String
    , _visualizerType :: String
    , _visualizerPath :: String
    } deriving (Eq, Generic, Show)

makeLenses ''VisualizerView

data VisualizationView = VisualizationView
    { _key                  :: String
    , _iframeId             :: String
    , _mode                 :: String
    , _currentVisualizer    :: VisualizerView
    , _selectedVisualizer   :: Maybe VisualizerName
    } deriving (Eq, Generic, Show)

makeLenses ''VisualizationView

data NodeVisualizationsView = NodeVisualizationsView
    { _nodeKey        :: String
    , _visualizations :: [VisualizationView]
    , _visualizers    :: [VisualizerView]
    } deriving (Eq, Generic, Show)

instance ToJSON VisualizerView where
    toEncoding = Lens.toEncoding
    toJSON     = Lens.toJSON

instance ToJSON VisualizationView where
    toEncoding = Lens.toEncoding
    toJSON     = Lens.toJSON

instance ToJSON NodeVisualizationsView where
    toEncoding = Lens.toEncoding
    toJSON     = Lens.toJSON

instance Convertible VisualizationId String where
    convert = show

instance Convertible Mode String where
    convert Focused    = "Focused"
    convert Preview    = "Preview"
    convert FullScreen = "FullScreen"
    convert _          = "Default"

instance Convertible VisualizerType String where
    convert InternalVisualizer = "InternalVisualizer"
    convert LunaVisualizer     = "LunaVisualizer"
    convert ProjectVisualizer  = "ProjectVisualizer"

instance Convertible Visualizer VisualizerView where
    convert v = VisualizerView
        {- visualizerName -} (convert
            $ v ^. Vis.visualizerId . Vis.visualizerName)
        {- visualizerType -} (convert
            $ v ^. Vis.visualizerId . Vis.visualizerType)
        {- visualizerPath -} (convert $ v ^. Vis.visualizerRelPath)

instance Convertible Visualization VisualizationView where
    convert v = VisualizationView
        {- key                -} (convert $ v ^. Vis.visualizationId)
        {- iframeId           -} (convert $ v ^. Vis.iframeId)
        {- mode               -} (convert $ v ^. Vis.mode)
        {- currentVisualizer  -} (convert $ v ^. Vis.visualizer)
        {- selectedVisualizer -} (convert
            <$> v ^? Vis.selectedVisualizerId . _Just . Vis.visualizerName)

instance Convertible (NodeLoc, NodeVisualizations) NodeVisualizationsView where
    convert (nl, nv) = NodeVisualizationsView
        {- nodeKey        -} (convert nl)
        {- visualizations -} (Map.elems $ convert <$> nv ^. Vis.visualizations)
        {- visualizations -} (convert . uncurry Vis.Visualizer
            <$> Map.toList (nv ^. Vis.visualizers))


foreign import javascript safe
    "atomCallback.getNodeEditorView().setVisualization($1)"
    setVisualization__ :: JSVal -> IO ()

foreign import javascript safe
    "atomCallback.getNodeEditorView().unsetVisualization($1)"
    unsetVisualization__ :: JSVal -> IO ()


visualizationView :: MonadIO m => DiffT NodeVisualizationsView m ()
visualizationView = diffApply setVisualization

setVisualization :: MonadIO m => NodeVisualizationsView -> m ()
setVisualization v = liftIO $ setVisualization__ =<< toJSONVal v

unsetVisualization :: MonadIO m => NodeVisualizationsView -> m ()
unsetVisualization v = liftIO $ unsetVisualization__ =<< toJSONVal v

nodeVisualizationsView :: MonadIO m
    => DiffT (Map NodeLoc NodeVisualizations) m ()
nodeVisualizationsView = diffMapWithKey
    (diffConvert visualizationView)
    (setVisualization . convert)
    (unsetVisualization . convert)
