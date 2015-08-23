module Dimple.Plot where

import           Utils.PreludePlus
import           Dimple.Types

foreign import javascript unsafe "dimple.plot.bubble" bubble :: IO Plot
foreign import javascript unsafe "dimple.plot.bar"    bar    :: IO Plot
foreign import javascript unsafe "dimple.plot.line"   line   :: IO Plot
foreign import javascript unsafe "dimple.plot.area"   area   :: IO Plot
