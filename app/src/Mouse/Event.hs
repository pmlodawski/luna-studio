module Mouse.Event where

import Data.Default
import Data.Maybe ( fromJust )

data KeyMods = KeyMods { shift :: Bool
                       , ctrl  :: Bool
                       , alt   :: Bool
                       , meta  :: Bool
                       } deriving (Eq, Show)

instance Default KeyMods where
  def = KeyMods False False False False

data Point = Point { _x :: Int
                   , _y :: Int
                   } deriving (Eq, Show)

-- fromTuple :: (Int, Int) -> Point
-- fromTuple t = Point (fst t) (snd t)

type Offset = Point

type FunctionNodeId = Int

data FunctionNode = FunctionNode { nodeId :: FunctionNodeId
                                 , selected :: Bool
                                 , position :: Point
                                 } deriving (Eq, Show)

-- makeLenses ''Point

-- p = Point 1 2

-- foo = p & x %~ (+1)
-- foo = p & x .~ 10

-- & = flip ($)

data Event = Pressed  Point KeyMods
           | Released Point KeyMods
           deriving (Eq, Show)

data ObjectEvent = NodeEvent Event FunctionNode
                 | NoObject Event
                 deriving (Eq, Show)

-- data NoObject = NoObject Event


new :: Bool -> Point -> KeyMods -> Event
new pressed mousePos keyMods = case pressed of
  True  -> Pressed  mousePos keyMods
  False -> Released mousePos keyMods

newWithNode :: Bool -> Point -> KeyMods -> FunctionNodeId -> Bool -> Point -> ObjectEvent
newWithNode pressed mousePos keyMods nodeId selected nodePos =
  NodeEvent (new pressed mousePos keyMods) (FunctionNode nodeId selected nodePos)

newWithNoObject :: Bool -> Point -> KeyMods -> ObjectEvent
newWithNoObject pressed mousePos keyMods = NoObject $ new pressed mousePos keyMods

newWithObject :: Bool -> Point -> KeyMods -> FunctionNodeId -> Bool -> Point -> ObjectEvent
newWithObject pressed mousePos keyMods nodeId selected nodePos =
  if nodeId < 0 then newWithNoObject pressed mousePos keyMods 
  else newWithNode pressed mousePos keyMods nodeId selected nodePos



selectObject :: ObjectEvent -> ObjectEvent
selectObject (NodeEvent ev node) = NodeEvent ev $ selectNode node
selectObject obj = obj

toggleObject :: ObjectEvent -> ObjectEvent
toggleObject (NodeEvent ev node) = NodeEvent ev $ toggleNode node
toggleObject obj = obj

selectNode :: FunctionNode -> FunctionNode
selectNode node = node { selected = True }

toggleNode :: FunctionNode -> FunctionNode
toggleNode node@(FunctionNode _ sel _) = node { selected = not sel }


unsafeGetNodeId :: ObjectEvent -> FunctionNodeId
unsafeGetNodeId = fromJust . getNodeId

getNodeId :: ObjectEvent -> Maybe FunctionNodeId
getNodeId (NodeEvent _ (FunctionNode nodeId _ _)) = Just nodeId
getNodeId _ = Nothing

unsafeGetNode :: ObjectEvent -> FunctionNode
unsafeGetNode = fromJust . getNode

getNode :: ObjectEvent -> Maybe FunctionNode
getNode (NodeEvent _ node) = Just node
getNode _ = Nothing

isNoObject :: ObjectEvent -> Bool
isNoObject (NoObject _) = True
isNoObject _ = False

isNodeSelected :: ObjectEvent -> Bool
isNodeSelected (NodeEvent _ (FunctionNode _ True _)) = True
isNodeSelected _ = False

isNodePressed :: ObjectEvent -> Bool
isNodePressed (NodeEvent (Pressed _ (KeyMods False False False False)) _) = True
isNodePressed _ = False

isUnselectedNodePressed :: ObjectEvent -> Bool
isUnselectedNodePressed (NodeEvent (Pressed _ (KeyMods False False False False)) (FunctionNode _ False _)) = True
isUnselectedNodePressed _ = False

isSelectedNodePressed :: ObjectEvent -> Bool
isSelectedNodePressed (NodeEvent (Pressed _ (KeyMods False False False False)) (FunctionNode _ True _)) = True
isSelectedNodePressed _ = False

isNodeTogglePressed :: ObjectEvent -> Bool
isNodeTogglePressed (NodeEvent (Pressed _ (KeyMods False False True False)) _) = True
isNodeTogglePressed _ = False

isNodeToggledOn :: ObjectEvent -> Bool
isNodeToggledOn (NodeEvent (Pressed _ (KeyMods False False True False)) (FunctionNode _ False _)) = True
isNodeToggledOn _ = False

isNodeToggledOff :: ObjectEvent -> Bool
isNodeToggledOff (NodeEvent (Pressed _ (KeyMods False False True False)) (FunctionNode _ True _)) = True
isNodeToggledOff _ = False

isMouseDown :: ObjectEvent -> Bool
isMouseDown (NodeEvent (Pressed _ (KeyMods False False False False)) _) = True
isMouseDown _ = False