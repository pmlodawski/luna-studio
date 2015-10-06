module Object.Port where

import           Utils.PreludePlus
import           Utils.Vector
import           Utils.Angle
import           Data.Fixed

import           JS.Camera
import           Object.Object


data ValueType = VTBool
               | VTInt
               | VTChar
               | VTFloat
               | VTString
               | VTVector ValueType
               | VTMaybe ValueType
               | VTNumeric
               | VTAny
               deriving (Ord, Eq, Show)

type ColorNum = Int

colorVT :: ValueType -> ColorNum
colorVT VTBool = 7
colorVT VTInt = 10
colorVT VTChar = 6
colorVT VTFloat = 3
colorVT VTString = 4
colorVT (VTVector _) = 9
colorVT (VTMaybe _) = 9
colorVT VTNumeric = 5
colorVT VTAny = 8


data DraggingTo = DraggingTo { _draggingTo :: Maybe (Vector2 Double) } deriving (Eq, Show)

data Location = Default
              | Connected NodeIdCollection DraggingTo
              deriving (Eq, Show)

data Port = Port { _portId        :: PortId
                 , _portValueType :: ValueType
                 , _angle         :: Angle
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

