module Flowbox.WSConnector.Cmd where

import           Flowbox.Prelude

data Cmd = Run { verbose :: Int }
         | Version
         deriving Show
