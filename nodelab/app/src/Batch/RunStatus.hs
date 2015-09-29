module Batch.RunStatus where

import Utils.PreludePlus
import Data.Int

data ProfileInfo = ProfileInfo { _nodeId        :: Int
                               , _totalCpuTime  :: Double
                               , _totalRealTime :: Int64
                               , _compileTime   :: Int64
                               , _executeTime   :: Int64
                               , _computeTime   :: Int64
                               } deriving (Show, Eq)

makeLenses ''ProfileInfo

data RunStatus = RunStatus { _profileInfos :: [ProfileInfo]
                           } deriving (Show, Eq)

makeLenses ''RunStatus
