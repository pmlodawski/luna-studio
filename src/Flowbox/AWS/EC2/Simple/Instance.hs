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

module Flowbox.AWS.EC2.Simple.Instance where

import qualified AWS.EC2.Types as Types

import           Flowbox.AWS.EC2.EC2               (EC2, EC2Resource)
import qualified Flowbox.AWS.EC2.Instance.Instance as Instance
import qualified Flowbox.AWS.EC2.Instance.Tag      as Tag
import qualified Flowbox.AWS.EC2.Simple.Tag        as Tag
import qualified Flowbox.AWS.User.User             as User
import           Flowbox.Prelude
import           Flowbox.System.Log.Logger



logger :: LoggerIO
logger = getLoggerIO "Flowbox.AWS.EC2.Simple.Instance"


findInstances :: EC2Resource m => EC2 m [Types.Instance]
findInstances = Instance.findInstances $ Tag.filter Tag.simpleKey [Tag.simpleValue]


getOrStart :: EC2Resource m
           => User.Name -> Types.RunInstancesRequest -> EC2 m Types.Instance
getOrStart userName instanceRequest = do
    let usable inst = state /= Types.InstanceStateTerminated && state /= Types.InstanceStateShuttingDown where state = Types.instanceState inst
    userInstances <- filter usable <$> findInstances
    case map Types.instanceState userInstances of
            []                           -> tag  =<< Instance.startNew userName instanceRequest
            [Types.InstanceStatePending] -> head <$> Instance.waitForStart (map Types.instanceId userInstances) def
            [Types.InstanceStateRunning] -> return $ head userInstances
            [Types.InstanceStateStopped] -> Instance.startExisting $ Types.instanceId $ head userInstances
            [_]                          -> tag  =<< Instance.startNew userName instanceRequest
            _                            -> undefined


tag :: EC2Resource m => Types.Instance -> EC2 m Types.Instance
tag inst = do Tag.tag Tag.simpleKey Tag.simpleValue [Types.instanceId inst]
              return inst
