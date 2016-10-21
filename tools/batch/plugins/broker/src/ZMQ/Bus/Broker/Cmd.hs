module ZMQ.Bus.Broker.Cmd where

import           Flowbox.Prelude



data Cmd = Serve { verbose :: Int
                 , noColor :: Bool
                 }
         | Version
         deriving Show
