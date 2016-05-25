module Empire.API.Persistence.Project where

import Prologue

import Data.Binary             (Binary)
import Data.IntMap.Lazy        (IntMap)
import Empire.API.Persistence.Library (Library)

type ProjectId = Int

data Project = Project { _name     :: String
                       , _libs     :: IntMap Library
                       } deriving (Show, Eq, Generic)

makeLenses ''Project

instance Binary Project
