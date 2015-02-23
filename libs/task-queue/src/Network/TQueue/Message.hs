{-# LANGUAGE DeriveGeneric #-}

module Message where

import GHC.Generics (Generic)
import Data.Binary (Binary, decode)
import Worker.Class (Worker(Worker))


data Message = RegisterWorker Worker
             deriving (Show, Generic)

instance Binary Message