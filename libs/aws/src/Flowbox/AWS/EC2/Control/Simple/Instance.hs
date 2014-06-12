---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Flowbox.AWS.EC2.Control.Simple.Instance where

import qualified AWS.EC2.Types          as Types
import           Control.Monad.IO.Class (liftIO)
import qualified Data.Time              as Time

import qualified Flowbox.AWS.EC2.Control.Simple.Tag  as Tag
import           Flowbox.AWS.EC2.EC2                 (EC2, EC2Resource)
import qualified Flowbox.AWS.EC2.Instance.Management as Management
import qualified Flowbox.AWS.EC2.Instance.Request    as Request
import qualified Flowbox.AWS.EC2.Instance.Tag        as Tag
import           Flowbox.AWS.Tag                     (Tag)
import qualified Flowbox.AWS.User.User               as User
import           Flowbox.Prelude                     hiding (error)
import           Flowbox.System.Log.Logger           hiding (info)


logger :: LoggerIO
logger = getLoggerIO "Flowbox.AWS.EC2.Control.Simple.Instance"


findInstances :: EC2Resource m => EC2 m [Types.Instance]
findInstances = Management.findInstances $ Tag.filter Tag.simpleKey [Tag.simpleValue]


getOrStartWait :: EC2Resource m
               => User.Name -> Types.RunInstancesRequest -> EC2 m [Types.Instance]
getOrStartWait userName instanceRequest = do
    let usable inst = state /= Types.InstanceStateTerminated && state /= Types.InstanceStateShuttingDown where state = Types.instanceState inst
    currentTime <- liftIO Time.getCurrentTime
    let tags = (Tag.simpleKey, Tag.simpleValue)
             : Tag.startTimeTag currentTime
             : [Tag.userTag $ Just userName]
    foundInstances <- filter usable <$> findInstances
    let foundInstancesCount = length foundInstances
        instancesStilToRun  = Types.runInstancesRequestMinCount instanceRequest - foundInstancesCount
        instancesInfo = map Just foundInstances ++ replicate instancesStilToRun Nothing
    instances <- mapM (prepareInstance tags instanceRequest) instancesInfo
    Management.waitForStart (map Types.instanceId instances) def


prepareInstance :: EC2Resource m
                => [Tag] -> Types.RunInstancesRequest -> Maybe Types.Instance -> EC2 m Types.Instance
prepareInstance tags instancesRequest instanceInfo = case instanceInfo of
    Nothing -> let request = Request.setAmount 1 instancesRequest
               in head <$> Management.startNew request tags
    Just info -> case Types.instanceState info of
        Types.InstanceStatePending   -> return info
        Types.InstanceStateRunning   -> return info
        Types.InstanceStateStopped   -> do Management.startExisting [Types.instanceId info] tags
                                           return info
        Types.InstanceStateStopping  -> fail "Instance is stopping, wait a moment and retry"
        _                            -> fail "Unknown instance state"
