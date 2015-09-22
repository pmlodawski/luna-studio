module Object.Port where

import           Utils.PreludePlus
import           Utils.Vector
import           Utils.Angle
import           Data.Fixed

import           JS.Camera
import           Object.Object


data ValueType = Int | Float | Char | Bool | String deriving (Eq, Show)



data DraggingTo = DraggingTo { _draggingTo :: Maybe (Vector2 Double) } deriving (Eq, Show)

data Location = Default
              | Connected NodeIdCollection DraggingTo
              deriving (Eq, Show)

data Port = Port { _portId     :: PortId
                 , _portType   :: ValueType
                 , _angle      :: Angle
                 } deriving (Eq, Show)

makeLenses ''Port

type PortCollection   = [Port]

instance PrettyPrinter ValueType where
    display = show


instance PrettyPrinter Port where
    display (Port ident tpe angle)
         = "n(" <> display ident
         <> " " <> display tpe
         <> " " <> display angle
         <> ")"


portDefaultAngle :: PortType -> Int -> PortId -> Angle
portDefaultAngle portType numPorts portId = angle `mod'` (2.0 * pi) where
    angle = (1 + fromIntegral portNum) * (pi / (fromIntegral $ numPorts + 1)) + delta
    portNum = portIdToNum portId
    delta = case portType of
        InputPort  ->       pi / 2.0
        OutputPort -> 3.0 * pi / 2.0

