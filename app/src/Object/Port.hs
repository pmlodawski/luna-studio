module Object.Port where

import           Utils.PreludePlus
import           Utils.Vector

import           JS.Camera
import           Object.Object


data ValueType = Int | Bool deriving (Eq, Show)

type PortId = ID

data Port = Port { _portId     :: PortId
                 , _tpe        :: ValueType
                 , _angle      :: Double
                 } deriving (Eq, Show)


makeLenses ''Port

instance PrettyPrinter ValueType where
    display = show


instance PrettyPrinter Port where
    display (Port ident tpe angle)
         = "n(" <> display ident
         <> " " <> display tpe
         <> " " <> display angle
         <> ")"

