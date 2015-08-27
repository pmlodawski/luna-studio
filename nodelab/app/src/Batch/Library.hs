module Batch.Library where

import Utils.PreludePlus

data Library = Library { _name :: String
                       , _path :: String
                       } deriving (Show, Eq)

makeLenses ''Library
