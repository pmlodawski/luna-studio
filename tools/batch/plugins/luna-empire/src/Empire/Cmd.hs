module Empire.Cmd where

import Flowbox.Prelude


data Cmd = Run { topics  :: [String]
               , verbose :: Int
               }
         | Version
         deriving Show

