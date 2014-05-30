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
import qualified Flowbox.AWS.EC2.Instance.Management as Instance
import qualified Flowbox.AWS.EC2.Instance.Tag        as Tag
import qualified Flowbox.AWS.User.User               as User
import           Flowbox.Prelude                     hiding (error)
import           Flowbox.System.Log.Logger



logger :: LoggerIO
logger = getLoggerIO "Flowbox.AWS.EC2.Control.Simple.Instance"


findInstances :: EC2Resource m => EC2 m [Types.Instance]
findInstances = Instance.findInstances $ Tag.filter Tag.simpleKey [Tag.simpleValue]


getOrStart :: EC2Resource m
           => User.Name -> Types.RunInstancesRequest -> EC2 m Types.Instance
getOrStart userName instanceRequest = do
    let usable inst = state /= Types.InstanceStateTerminated && state /= Types.InstanceStateShuttingDown where state = Types.instanceState inst
    currentTime <- liftIO $ Time.getCurrentTime
    let tags = (Tag.simpleKey, Tag.simpleValue)
             : Tag.startTimeTag currentTime
             : [Tag.userTag $ Just userName]
    userInstances <- filter usable <$> findInstances
    let instancesCount = length userInstances
    if instancesCount == 0
        then Instance.startNew instanceRequest tags
        else do if instancesCount > 1
                    then logger error "Too many instances are started, but continuing anyway"
                    else return ()
                case head $ map Types.instanceState userInstances of
                    Types.InstanceStatePending   -> head <$> Instance.waitForStart (map Types.instanceId userInstances) def
                    Types.InstanceStateRunning   -> return $ head userInstances
                    Types.InstanceStateStopped   -> Instance.startExisting (Types.instanceId $ head userInstances) tags
                    Types.InstanceStateStopping  -> fail "Instance is stopping, wait a moment and retry"
                    _                            -> fail "Unknown instance state"
