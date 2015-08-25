module Batch.Project where

import Utils.PreludePlus
import Data.Int

-- FIXME[Marcin Kostrzewa]: Merge with Project definition from libs/batch
data Project = Project { _name :: Maybe String
                       , _path :: String
                       , _id   :: Int32
                       } deriving (Show, Eq)

makeLenses ''Project
