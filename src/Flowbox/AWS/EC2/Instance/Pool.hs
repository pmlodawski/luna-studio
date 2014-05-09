---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}

module Flowbox.AWS.EC2.Instance.Pool where

import Control.Concurrent.MVar (MVar)
import Data.Map                (Map)
import Data.Set                (Set)

import           Flowbox.AWS.EC2.EC2               (EC2, EC2Resource)
import qualified Flowbox.AWS.EC2.Instance.Instance as Instance
import qualified Flowbox.AWS.User.User             as User
import           Flowbox.Prelude
import           Flowbox.System.Log.Logger



logger :: LoggerIO
logger = getLoggerIO "Flowbox.AWS.Instance.Pool"


data PoolData = PoolData { used :: Map Instance.ID User.Name
                         , free :: Set Instance.ID
                         } deriving (Show, Read)


type Pool = MVar PoolData


initialize :: EC2Resource m => EC2 m Pool
initialize = undefined


release :: EC2Resource m => Instance.ID -> Pool -> EC2 m Pool
release instanceID pool = do
    undefined


retrieve :: EC2Resource m => Pool -> EC2 m (Instance.ID, Pool)
retrieve = undefined


freeUnused :: EC2Resource m => Pool -> EC2 m Pool
freeUnused = undefined


monitor :: EC2Resource m => Pool -> EC2 m ()
monitor = undefined
