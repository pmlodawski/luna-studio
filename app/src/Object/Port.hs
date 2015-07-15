module Object.Port where

import           Control.Applicative
import           Control.Lens
import           Data.Dynamic
import           Data.Monoid
import           Data.Maybe    ( isJust, catMaybes )

import           JS.Camera
import           Object.Object
import           Utils.Vector
import           Utils.Wrapper
import           Utils.PrettyPrinter


data ValueType = Int | Bool deriving (Eq, Show)

type PortId = ID

data Port = Port { _portId     :: PortId
                 , _input      :: Bool
                 , _tpe        :: ValueType
                 , _angle      :: Double
                 } deriving (Eq, Show)


makeLenses ''Port

instance PrettyPrinter ValueType where
    display = show


instance PrettyPrinter Port where
    display (Port ident input tpe angle) = "n(" <> display ident
                                         <> " " <> display input
                                         <> " " <> display tpe
                                         <> " " <> display angle
                                         <> ")"

