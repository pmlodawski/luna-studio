module Batch.Library where

import Utils.PreludePlus
import Data.Int
import Data.Aeson (ToJSON)

data Library = Library { _name :: String
                       , _path :: String
                       , _id   :: Int32
                       } deriving (Show, Eq, Generic)

makeLenses ''Library

instance ToJSON Library
