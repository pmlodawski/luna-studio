module Batch.Library where

import Utils.PreludePlus
import Data.Int

data Library = Library { _name :: String
                       , _path :: String
                       , _id   :: Int32
                       } deriving (Show, Eq)

makeLenses ''Library
