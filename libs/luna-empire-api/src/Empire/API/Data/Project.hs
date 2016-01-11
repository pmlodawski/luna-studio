module Empire.API.Data.Project where

import Prologue

import Data.Binary (Binary)
import Data.IntMap.Lazy (IntMap)
import Empire.API.Data.Library (Library)

type ProjectId = Int

data Project = Project { _name     :: Maybe String
                       , _path     :: String
                       , _libs     :: IntMap Library
                       } deriving (Show, Eq, Generic)

makeLenses ''Project
instance Binary Project
