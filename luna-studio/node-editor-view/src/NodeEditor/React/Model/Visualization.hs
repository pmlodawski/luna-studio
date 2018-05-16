{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE StrictData        #-}
module NodeEditor.React.Model.Visualization
    ( module NodeEditor.React.Model.Visualization
    , module X
    ) where

import           Common.Prelude
import           Data.Map                      (Map)
import qualified Data.Map                      as Map
import           LunaStudio.Data.NodeLoc       (NodeLoc)
import           Data.UUID.Types  (UUID)
import           LunaStudio.Data.Visualization as X (VisualizationId, VisualizationValue (..))
import           LunaStudio.Data.Visualizer    as X (Visualizer (Visualizer), VisualizerId (VisualizerId), VisualizerMatcher,
                                                     VisualizerName, VisualizerPath, VisualizerType (..), errorVisId, getMdVisualizer,
                                                     placeholderVisId, visualizerId, visualizerName, visualizerRelPath, visualizerType,
                                                     _InternalVisualizer, _LunaVisualizer, _ProjectVisualizer)


type IframeId = UUID

data Mode
    = Default
    | Focused
    | Preview
    | FullScreen
    | Hidden
    deriving (Eq, Generic, Show)

instance Default Mode where def = Default

data Parent
    = Node NodeLoc
    | Searcher
    deriving (Eq, Generic, Show)

data Visualization = Visualization
    { _visualizationId      :: VisualizationId
    , _iframeId             :: UUID
    , _mode                 :: Mode
    , _visualizer           :: Visualizer
    , _selectedVisualizerId :: Maybe VisualizerId
    } deriving (Eq, Generic, Show)

data NodeVisualizations = NodeVisualizations
    { _dataVisualizations :: Map VisualizationId Visualization
    , _errorVisualization :: Maybe Visualization
    , _visualizers        :: Map VisualizerId VisualizerPath
    } deriving (Eq, Generic, Show)


data VisualizationProperties = VisualizationProperties
    { _visPropNodeLoc        :: NodeLoc
    , _visPropIsNodeExpanded :: Bool
    , _visPropArgPortsNumber :: Int
    , _visPropVisualizers    :: Map VisualizerId VisualizerPath
    , _visPropVisualization  :: Visualization
    } deriving (Eq, Generic, Show)

makePrisms ''Mode
makePrisms ''Parent
makeLenses ''Visualization
makeLenses ''NodeVisualizations
makeLenses ''VisualizationProperties

instance Default NodeVisualizations where def = NodeVisualizations def def def
instance NFData Mode
instance NFData Parent
instance NFData Visualization
instance NFData NodeVisualizations
instance NFData VisualizationProperties

visualizations :: Getter NodeVisualizations (Map VisualizationId Visualization)
visualizations = to getter where
    getter nv = maybe
        id
        (\errVis -> Map.insert (errVis ^. visualizationId) errVis)
        (nv ^. errorVisualization)
        $ nv ^. dataVisualizations

activeVisualizations :: Getter NodeVisualizations (Map VisualizationId Visualization)
activeVisualizations = to getter where
    getter nv = Map.filter ((Hidden /=) . view mode) $ nv ^. visualizations


-- toIdleVisualization :: VisualizationStatus -> RunningVisualization -> IdleVisualization
-- toIdleVisualization vs = IdleVisualization vs . view (visualizerProperties . selectedVisualizerId)

-- stopVisualizations :: NodeVisualizations -> NodeVisualizations
-- stopVisualizations nodeVis = nodeVis & visualizations     .~ def
--                                      & idleVisualizations .~ (nodeVis ^. idleVisualizations) <> (map (toIdleVisualization Ready) . Map.elems $ nodeVis ^. visualizations)


awaitingDataMsg, noVisMsg, noDataMsg :: Text
awaitingDataMsg = "AWAITING_DATA"
noVisMsg        = "NO_VIS_FOR_TYPE"
noDataMsg       = "NO_DATA"
