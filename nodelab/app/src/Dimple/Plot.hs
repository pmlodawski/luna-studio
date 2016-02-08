module Dimple.Plot where

import           Utils.PreludePlus
import           Dimple.Types

foreign import javascript safe "dimple.plot.bubble" bubble :: IO Plot
foreign import javascript safe "dimple.plot.bar"    bar    :: IO Plot
foreign import javascript safe "dimple.plot.line"   line   :: IO Plot
foreign import javascript safe "dimple.plot.area"   area   :: IO Plot
