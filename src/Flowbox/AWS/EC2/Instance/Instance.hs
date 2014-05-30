---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}
module Flowbox.AWS.EC2.Instance.Instance where

import qualified Data.IP   as IP
import           Data.Text (Text)

import qualified Flowbox.Data.Time as Time
import           Flowbox.Prelude   hiding (id)



type ID = Text


data Status = Running
            | Stopped
            deriving (Show, Ord, Eq, Read)


data Instance = Instance { _id      :: ID
                         , _ip_addr :: IP.IPv4
                         , _started :: Time.UTCTime
                         , _status  :: Status
                         } deriving (Show, Ord, Eq, Read)

makeLenses (''Instance)
