{-# LANGUAGE ExistentialQuantification #-}
module NodeEditor.State.Action where

import           Common.Prelude
import           Data.Aeson                           (FromJSON, ToJSON)
import           Data.Curve                           (Curve)
import           Data.Dynamic
import           Data.Map                             (Map)
import           Data.Set                             (Set)
import qualified Data.Set                             as Set
import           Data.Time.Clock                      (UTCTime)
import           LunaStudio.Data.NodeLoc              (NodeLoc)
import           LunaStudio.Data.PortRef              (AnyPortRef, InPortRef, OutPortRef)
import           LunaStudio.Data.Position             (Position)
import           LunaStudio.Data.ScreenPosition       (ScreenPosition)
import           NodeEditor.Data.Slider               (InitValue)
import           NodeEditor.React.Model.Connection    (ConnectionId)
import           NodeEditor.React.Model.InputField    (ActiveInputField, InputFieldId)
import           NodeEditor.React.Model.Visualization (VisualizationId, VisualizationMode)

data NodeDrag = NodeDrag { _nodeDragStartPos      :: Position
                         , _nodeDragNodeLoc       :: NodeLoc
                         , _nodeDragNodesStartPos :: Map NodeLoc Position
                         , _nodeDragSnappedConnId :: Maybe ConnectionId
                         } deriving (Eq, Show, Generic, Typeable)

makeLenses ''NodeDrag

data MultiSelection = MultiSelection { _multiSelecectionStartPos :: Position
                                     } deriving (Eq, Show, Generic, Typeable)

makeLenses ''MultiSelection

data PanDrag = PanDrag { _panDragPreviousPos :: ScreenPosition
                       } deriving (Eq, Show, Generic, Typeable)

makeLenses ''PanDrag

data ZoomDrag = ZoomDrag { _zoomDragFixedPoint  :: ScreenPosition
                         , _zoomDragPreviousPos :: ScreenPosition
                         } deriving (Eq, Show, Generic, Typeable)

makeLenses ''ZoomDrag


data SliderDrag = SliderDrag { _sliderDragPortRef   :: InPortRef
                             , _sliderDragStartTime :: UTCTime
                             , _sliderDragInitValue :: InitValue
                             } deriving (Eq, Show, Generic, Typeable)

makeLenses ''SliderDrag

data PenConnect = PenConnect { _penConnectCurve           :: Curve
                             , _penConnectLastVisitedNode :: Maybe NodeLoc
                             } deriving (Eq, Generic, Show, Typeable)

makeLenses ''PenConnect

data PenDisconnect = PenDisconnect { _penDisconnectCurve               :: Curve
                                   , _penDisconnectLastVisitedNode     :: Maybe NodeLoc
                                   , _penDisconnectNextNodeRestriction :: Maybe NodeLoc
                                   } deriving (Eq, Generic, Show, Typeable)

makeLenses ''PenDisconnect

data Mode = Drag | Click deriving (Eq, Generic, Show, Typeable)

data Connect = Connect { _connectStartPos              :: ScreenPosition
                       , _connectSourcePort            :: AnyPortRef
                       , _connectIsConnModified        :: Bool
                       , _connectSnappedPort           :: Maybe AnyPortRef
                       , _connectIsArgumentConstructor :: Bool
                       , _connectMode                  :: Mode
                       } deriving (Eq, Generic, Show, Typeable)

makeLenses ''Connect

data PortDrag = PortDrag { _portDragStartPos              :: ScreenPosition
                         , _portDragPortStartPosInSidebar :: Position
                         , _portDragStartPortRef          :: OutPortRef
                         , _portDragActPortRef            :: OutPortRef
                         , _portDragIsArgumentConstructor :: Bool
                         , _portDragMode                  :: Mode
                         } deriving (Eq, Generic, Show, Typeable)

makeLenses ''PortDrag

data Searcher = Searcher deriving (Eq, Generic, Show, Typeable)

makeLenses ''Searcher

data VisualizationDrag = VisualizationDrag { _visNodeLoc :: NodeLoc
                                           , _visIdx     :: Int
                                           , _visPos     :: Position
                                           } deriving (Eq, Show, Generic, Typeable)

makeLenses ''VisualizationDrag


data VisualizationActive = VisualizationActive { _visualizationActiveNodeLoc         :: NodeLoc
                                               , _visualizationActiveVisualizationId :: VisualizationId
                                               , _visualizationActiveSelectedMode    :: VisualizationMode
                                               , _visualizationActiveTriggeredByVis  :: Bool
                                               } deriving (Eq, Show, Generic, Typeable)

makeLenses ''VisualizationActive

data SidebarAddRemoveMode = SidebarAddRemoveMode { _sidebarAddRemoveModeNodeLoc :: NodeLoc
                                                 } deriving (Eq, Show, Generic, Typeable)

makeLenses ''SidebarAddRemoveMode

data InputFieldActive = InputFieldActive { _inputFieldActiveFieldId :: InputFieldId
                                         , _inputFieldActiveState   :: ActiveInputField
                                         } deriving (Eq, Show, Generic, Typeable)

makeLenses ''InputFieldActive

data SomeAction m = forall a. (Action m a, Show a, Typeable a) => SomeAction Dynamic a deriving (Typeable)

instance Show (SomeAction m) where
    show (SomeAction _ a) = show a


class (Typeable a, Show a, Monad m) => Action m a where
    begin :: a -> m ()
    continue :: (a -> m ()) -> m ()
    update :: a -> m ()
    end :: a -> m ()

instance (Typeable m, Monad m) => Action m (SomeAction m) where
    begin  (SomeAction _ a) = begin a
    continue _              = return ()
    update (SomeAction _ a) = update a
    end    (SomeAction _ a) = end a

someAction :: Action m a => a -> SomeAction m
someAction a = SomeAction (toDyn a) a

fromSomeAction :: Typeable a => SomeAction m -> Maybe a
fromSomeAction (SomeAction d _) = fromDynamic d


newtype ActionRep = ActionRep TypeRep deriving (Show, Eq, Ord)

connectAction, inputFieldActiveAction, multiSelectionAction, nodeDragAction, panDragAction, penConnectAction, penDisconnectAction, portDragAction, searcherAction, sidebarAddRemoveModeAction, sliderDragAction, visualizationActiveAction, visualizationDragAction, zoomDragAction :: ActionRep
connectAction              = ActionRep (typeOf Connect)
inputFieldActiveAction     = ActionRep (typeOf InputFieldActive)
multiSelectionAction       = ActionRep (typeOf MultiSelection)
nodeDragAction             = ActionRep (typeOf NodeDrag)
panDragAction              = ActionRep (typeOf PanDrag)
penConnectAction           = ActionRep (typeOf PenConnect)
penDisconnectAction        = ActionRep (typeOf PenDisconnect)
portDragAction             = ActionRep (typeOf PortDrag)
searcherAction             = ActionRep (typeOf Searcher)
sidebarAddRemoveModeAction = ActionRep (typeOf SidebarAddRemoveMode)
sliderDragAction           = ActionRep (typeOf SliderDrag)
visualizationActiveAction  = ActionRep (typeOf VisualizationActive)
visualizationDragAction    = ActionRep (typeOf VisualizationDrag)
zoomDragAction             = ActionRep (typeOf ZoomDrag)

overlappingActions :: [Set ActionRep]
overlappingActions = [ Set.fromList [ connectAction
                                    , inputFieldActiveAction
                                    , multiSelectionAction
                                    , nodeDragAction
                                    , penConnectAction
                                    , penDisconnectAction
                                    , portDragAction
                                    , searcherAction
                                    , sidebarAddRemoveModeAction
                                    , sliderDragAction
                                    , visualizationActiveAction
                                    , visualizationDragAction
                                    ]
                     , Set.fromList [ panDragAction
                                    , portDragAction
                                    , visualizationActiveAction
                                    , zoomDragAction
                                    ]
                     ]

actionsBlockingPortHighlight :: Set ActionRep
actionsBlockingPortHighlight = Set.fromList [ multiSelectionAction
                                            , nodeDragAction
                                            , panDragAction
                                            , penConnectAction
                                            , penDisconnectAction
                                            , portDragAction
                                            , searcherAction
                                            , sliderDragAction
                                            , visualizationDragAction
                                            , zoomDragAction
                                            ]

actionsClosingOnMouseLeave :: Set ActionRep
actionsClosingOnMouseLeave = Set.fromList [ multiSelectionAction
                                          , nodeDragAction
                                          , panDragAction
                                          , penConnectAction
                                          , penDisconnectAction
                                          , portDragAction
                                          , sliderDragAction
                                          , visualizationDragAction
                                          , zoomDragAction
                                          ]
