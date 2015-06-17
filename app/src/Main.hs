module Main where

import Control.Applicative
import Control.Monad       ( when )
import Data.Default
import Data.Char           ( chr )
import Data.Monoid         ( (<>) )
import Data.Maybe          ( fromJust )
import Debug.Trace         ( trace )

import Reactive.Banana ( Moment
                       , (<$>)
                       , (<*>)
                       , (<$)
                       , compile
                       , accumB
                       , accumE
                       , stepper
                       , filterE
                       , union
                       )
import Reactive.Banana.Frameworks ( AddHandler(..)
                                  , Frameworks
                                  , liftIO 
                                  , fromAddHandler
                                  , reactimate
                                  , reactimate'
                                  , actuate
                                  , initial
                                  , changes
                                  )
import Reactive.Banana.Utils
import Reactive.Handlers
import JS.Bindings
import Mouse.Event ( Point(..), KeyMods(..), FunctionNode(..), FunctionNodeId, Event, ObjectEvent(..) )
import qualified Mouse.Event

reactKey :: Char -> IO ()
reactKey keyCode = case keyCode of
  'u' -> unselectAllNodes -- not working properly
  _   -> return ()

-- moveNode :: MouseHandled -> Point -> IO ()
-- moveNode nodeDragged@(NodePressed (NodeOffset id (Point offx offy)) (MouseDownState (MousePosMods (Point x y) mods))) (Point mx my) =
--   when (id >= 0) $ do 
--     -- print nodeDragged
--     dragNode id (mx + offx) (my - offy) 
-- moveNode _ _ = return ()

-- moveNodes :: HandledWithMouse -> IO ()
-- moveNodes handledWithMouse@(HandledWithMouse mouseHandledList mp) = do
--   -- print handledWithMouse
--   flip mapM_ mouseHandledList $ \mouseHandled -> do
--     moveNode mouseHandled mp
-- -- moveNodes _ = return ()

accumMouseNodes :: [ObjectEvent] -> [ObjectEvent] -> [ObjectEvent]
accumMouseNodes new old =
  trace ("--- new " <> (show new) <> "\n--- old " <> (show old) <> " ---" ) $
    case head new of
      -- NodePressed (NodeOffset nodeId (Point offx offy)) (MouseDownState (MousePosMods (Point x y) mods)) ->
      --   case mods of
      --     KeyMods False False True False -> new <> old
      --     _ -> new
      -- NodeReleased (NodeOffset nodeId (Point offx offy)) (MouseUpState (MousePosMods (Point x y) mods)) ->
      --   case mods of
      --     KeyMods False False True False -> new <> old
      --     _ -> new
      _ -> new


hasNodeId :: Int -> ObjectEvent -> Bool
hasNodeId filteredNodeId (NodeEvent _ (FunctionNode fnodeId _ _)) = filteredNodeId == fnodeId
hasNodeId _ _ = False

removeObj :: ObjectEvent -> [ObjectEvent] -> [ObjectEvent]
removeObj obj objects =
  filter (\objI -> not $ hasNodeId (Mouse.Event.unsafeGetNodeId obj) objI) objects

accumNodesSelected :: [ObjectEvent] -> [ObjectEvent] -> [ObjectEvent]
accumNodesSelected (newObj:_) oldObjs
    | Mouse.Event.isNoObject              newObj = []
    | Mouse.Event.isSelectedNodePressed   newObj = newSelectedObj : oldFilteredObjs
    | Mouse.Event.isUnselectedNodePressed newObj = newSelectedObj : []
    | Mouse.Event.isNodeToggledOff        newObj = oldFilteredObjs
    | Mouse.Event.isNodeToggledOn         newObj = newSelectedObj : oldFilteredObjs
    | otherwise                                  = []
    where
      newSelectedObj = Mouse.Event.selectObject newObj
      oldFilteredObjs = removeObj newObj oldObjs


selectNewNode :: FunctionNode -> IO ()
selectNewNode node = unselectAllNodes >> updateNode node

updateNode :: FunctionNode -> IO ()
updateNode (FunctionNode nodeId True  _) = setNodeSelected   nodeId
updateNode (FunctionNode nodeId False _) = setNodeUnselected nodeId


focusFirstNode :: [FunctionNode] -> IO ()
focusFirstNode ((FunctionNode nodeId _ _):_) = unfocusNode >> setNodeFocused nodeId
focusFirstNode _                             = unfocusNode

-- selectNode :: FunctionNode -> IO ()
-- selectNode node@(FunctionNode nodeId selected _) = do
--   -- nodeSelected <- isNodeSelected nodeId
--   -- when (not nodeSelected) $ do
--   --   unselectAllNodes
--     setNodeSelected nodeId

-- selectNodes :: [FunctionNode] -> IO ()
-- selectNodes nodes = do
--   unselectAllNodes
--   mapM_ selectNode nodes

-- rmdups :: (Ord a) => [a] -> [a]
-- rmdups = map head . group . sort

data Drag = Drag Bool [FunctionNode] (Point, Point) deriving (Eq, Show)


moveNodes :: [FunctionNode] -> IO ()
moveNodes nodes = do
  flip mapM_ nodes $ \node -> do
    let Point x y = position node
    dragNode (nodeId node) x y

performDrag :: Drag -> [FunctionNode]
performDrag drag@(Drag pressed nodes (d, p)) = if not pressed then [] else flip fmap nodes $
  \(FunctionNode nodeId selected (Point x y)) -> FunctionNode nodeId selected (Point (_x p) (_y p)) -- wrong

makeNetworkDescription :: Frameworks t
                       => VNodePresentation
                       -> Moment t ()
makeNetworkDescription container = do
  mouseClickedE <- fromAddHandler mouseClickedHandler
  mouseDownE    <- fromAddHandler mouseDownHandler   
  mouseUpE      <- fromAddHandler mouseUpHandler
  keyPressedE   <- fromAddHandler keyPressedHandler
  mouseMovedE   <- fromAddHandler mouseMovedHandler

  let clicksCountB       = accumB 0 ((+ 1) <$ mouseClickedE)
      lastKeyB           = stepper (chr 0) keyPressedE
      mousePosB          = stepper (Point 0 0) mouseMovedE


      mousePressedB       = stepper False (Mouse.Event.isMouseDown <$> (mouseDownE `union` mouseUpE))

      noObjectSelectedE   = filterE Mouse.Event.isNoObject mouseDownE
      oldNodeSelectedObjE = filterE Mouse.Event.isSelectedNodePressed mouseDownE
      newNodeSelectedObjE = filterE Mouse.Event.isUnselectedNodePressed mouseDownE
      nodeToggledObjE     = filterE Mouse.Event.isNodeTogglePressed mouseDownE

      newSelectionObjE      = noObjectSelectedE `union` newNodeSelectedObjE `union` oldNodeSelectedObjE `union` nodeToggledObjE
      -- nodeSelectedE      = newNodeSelectedObjE

      -- perform changes

      oldNodeSelectedPE   = Mouse.Event.unsafeGetNode <$> oldNodeSelectedObjE
      newNodeSelectedPE   = Mouse.Event.unsafeGetNode <$> (Mouse.Event.selectObject <$> newNodeSelectedObjE)
      nodeToggledPE       = Mouse.Event.unsafeGetNode <$> (Mouse.Event.toggleObject <$> nodeToggledObjE)

      -- nodeSelectedPE      =


      -- nodeSelectedE     = filterE Mouse.Event.isNodePressed mouseDownE

      nodesSelectedObjPB     = accumB [] $ accumNodesSelected <$> (pure <$> newSelectionObjE)
      nodesSelectedPB        = (fmap Mouse.Event.unsafeGetNode) <$> nodesSelectedObjPB


      -- nodesSelectedB    = accumB [] (accumNodesSelected <$> mouseDownE)
      
      -- mouseDragB        = accumB [] $ accumMouseNodes <$> (pure <$> (mouseDownE `union` mouseUpE))
      -- nodeMoveB         = HandledWithMouse <$> mouseDragB <*> mousePosB

      mouseDragB        = accumB (Point 0 0, Point 0 0) $ (\(_, new) (_, old) -> (Point (_x new - _x old) (_y new - _y old), new) ) 
        <$> ((\p -> (p, p)) <$> mouseMovedE)


      nodesMovePB        = Drag <$> mousePressedB <*> nodesSelectedPB <*> mouseDragB
      nodesMovedPPB      = performDrag <$> nodesMovePB

      documentClicksB   = ("Clicks: " <>) . show <$> clicksCountB
      documentLastKeyB  = (" Key pressed: " <>) . pure <$> lastKeyB
      documentPositionB = (" Position: " <>) . show <$> mousePosB
      documentB         = documentClicksB <<*>> documentLastKeyB <<*>> documentPositionB

  -- Render the initial view
  -- initial documentB >>= liftIO . logAs "Render "

  -- docCh <- changes documentB
  -- mouseMoveCh <- changes nodeMoveB

  -- reactimate' $ fmap (logAs "Render ") <$> docCh

  reactimate $ reactKey <$> keyPressedE

  -- reactimate $ updateNode <$> nodeSelectedE
  -- reactimate $ (logAs "nodeSelectedE ") <$> nodeSelectedE

  -- reactimate $ (logAs "mouseDrag ") <$> mouseDragE

  reactimate $ selectNewNode <$> newNodeSelectedPE
  -- reactimate $ (logAs "newNodeSelectedPE ") <$> newNodeSelectedPE

  reactimate $ updateNode <$> nodeToggledPE
  -- reactimate $ (logAs "nodeToggledPE ") <$> nodeToggledPE


  nodesSelectedPFE <- changes $ nodesSelectedPB
  

  reactimate' $ (fmap focusFirstNode) <$> nodesSelectedPFE
  -- reactimate $ focusFirstNode <$> ((fmap Mouse.Event.unsafeGetNode) <$> nodesSelectedPE)
  -- reactimate $ (logAs "nodeToggledPE ") <$> nodeToggledPE


  -- reactimate $ (logAs "newSelectionE ") <$> (Mouse.Event.getNodeId <$> newSelectionE)

  reactimate' $ (fmap $ logAs "nodesSelectedPFE ") <$> nodesSelectedPFE
  -- reactimate $ (logAs "nodesSelectedE ") <$> ((fmap Mouse.Event.unsafeGetNodeId) <$> nodesSelectedE)

  -- reactimate $ selectNode <$> nodeSelectedDownE
  -- reactimate $ (logAs "nodeSelectedDownE ") <$> nodeSelectedDownE

  -- reactimate $ toggleNodeSelection <$> (nodeId <$> nodeToggleSelectedDownE)
  -- reactimate $ (logAs "nodeToggleSelectedDownE ") <$> nodeToggleSelectedDownE

  reactimate $ unselectAllNodes <$ noObjectSelectedE
  -- reactimate $ (logAs "noObjectSelectedE " ()) <$ noObjectSelectedE

  nodesMovePFE <- changes nodesMovePB
  nodesMovedPPFE <- changes nodesMovedPPB

  -- reactimate' $ (fmap $ logAs "nodesMovePFE ") <$> nodesMovePFE
  reactimate' $ (fmap $ logAs "nodesMovedPPFE ") <$> nodesMovedPPFE
  reactimate' $ fmap moveNodes <$> nodesMovedPPFE



main :: IO ()
main = do
  container <- newTopLevelContainer

  -- Main.init
  -- Main.create(30)
  -- Main.render

  eventNetwork <- compile $ makeNetworkDescription container

  actuate eventNetwork

