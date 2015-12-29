module Empire.Binary.Instances.Missing where

import Data.Binary (Binary)
import System.Path (Path, Node, Mount)

instance Binary Path
instance Binary Mount
instance Binary Node
