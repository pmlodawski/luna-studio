module Reactive.State.ConnectionPen where


import           Utils.PreludePlus
import           Utils.Vector

import           Object.Object


data DrawingType = Connecting | Disconnecting deriving (Show, Eq)

data Drawing = Drawing { _previousPos  :: Vector2 Int
                       , _drawingType  :: DrawingType
                       , _lastNode     :: Maybe NodeId
                       , _visitedNodes :: [NodeId]
                       } deriving (Eq, Show)


data State = State { _drawing  :: Maybe Drawing
                   } deriving (Eq, Show)


makeLenses ''State
makeLenses ''Drawing


instance Default State where
    def = State def

instance PrettyPrinter State where
    display (State drawing) = "dS(" <> display drawing <> ")"

instance PrettyPrinter Drawing where
    display (Drawing pos tpe node visited) = display pos <> " " <> display tpe <> " " <> display node <> " / " <> display visited

instance PrettyPrinter DrawingType where
    display = show
