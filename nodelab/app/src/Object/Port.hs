module Object.Port where

import           Utils.PreludePlus
import           Utils.Vector
import           Utils.Angle
import           Data.Fixed

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
               | VTObject
               deriving (Ord, Eq, Show)

type ColorNum = Int

colorVT :: ValueType -> ColorNum
colorVT VTBool = 7
colorVT VTInt = 11
colorVT VTChar = 6
colorVT VTFloat = 3
colorVT VTString = 4
colorVT (VTVector _) = 9
colorVT (VTMaybe _) = 9
colorVT VTNumeric = 10
colorVT VTAny = 8
colorVT VTObject = 8

colorError :: ColorNum
colorError = 13


data DraggingTo = DraggingTo { _draggingTo :: Maybe (Vector2 Double) } deriving (Eq, Show)

data Location = Default
              | Connected NodeIdCollection DraggingTo
              deriving (Eq, Show)

data Port = Port { _portId        :: PortId
                 , _portValueType :: ValueType
                 } deriving (Eq, Show)

makeLenses ''Port

type PortCollection   = [Port]

instance PrettyPrinter ValueType where
    display = show


instance PrettyPrinter Port where
    display (Port ident tpe)
         = "n(" <> display ident
         <> " " <> display tpe
         <> ")"


portDefaultAngle :: PortType -> Int -> PortId -> Vector2 Double
portDefaultAngle portType numPorts portId = (/ 10.0) <$> Vector2 (cos angleMod) (sin angleMod) where
    angleMod = angle `mod'` (2.0 * pi)
    angle = (1 + fromIntegral portNum) * (pi / (fromIntegral $ numPorts + 1)) + delta
    portNum = portIdToNum portId
    delta = case portType of
        InputPort  ->       pi / 2.0
        OutputPort -> 3.0 * pi / 2.0

