module Batch.Project where

import Utils.PreludePlus
import Data.Int
import Batch.Library
import Data.Aeson (ToJSON)

-- FIXME[Marcin Kostrzewa]: Merge with Project definition from libs/batch
data Project = Project { _name :: Maybe String
                       , _path :: String
                       , _id   :: Int32
                       , _libs :: [Library]
                       } deriving (Show, Eq, Generic)

makeLenses ''Project
instance ToJSON Project
