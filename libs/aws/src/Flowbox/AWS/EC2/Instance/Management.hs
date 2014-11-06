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
{-# LANGUAGE TemplateHaskell       #-}

module Flowbox.AWS.EC2.Instance.Management where

import qualified AWS.EC2.Types          as Types
import qualified AWS.EC2.Util           as Util
import qualified Control.Concurrent     as Concurrent
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Control.Monad.Loops    as Loops
import qualified System.IO              as IO

import           Flowbox.AWS.EC2.EC2               (EC2, EC2Resource)
import qualified Flowbox.AWS.EC2.EC2               as EC2
import qualified Flowbox.AWS.EC2.Instance.Instance as Instance
import qualified Flowbox.AWS.EC2.Instance.Tag      as Tag
import           Flowbox.AWS.EC2.Instance.WaitTime (WaitTimes)
import qualified Flowbox.AWS.EC2.Instance.WaitTime as WaitTime
import           Flowbox.AWS.Tag                   (Tag)
import           Flowbox.Prelude
import           Flowbox.System.Log.Logger



logger :: LoggerIO
logger = getLoggerIO $(moduleName)


ready :: Types.Instance -> Bool
ready = hasState Types.InstanceStateRunning


resumable :: Types.Instance -> Bool
resumable inst = hasState Types.InstanceStateRunning  inst
              || hasState Types.InstanceStateStopped  inst
              || hasState Types.InstanceStateStopping inst


hasState :: Types.InstanceState -> Types.Instance -> Bool
hasState state inst = Types.instanceState inst == state


startNew :: EC2Resource m
         => Types.RunInstancesRequest -> [Tag] -> EC2 m [Types.Instance]
startNew instanceRequest tags = do
    logger info "Starting new instance..."
    reservation <- EC2.runInstances instanceRequest
    let instances   = Types.reservationInstanceSet reservation
        instanceIDs = map Types.instanceId instances
    Tag.tag tags instanceIDs
    logger info "Starting new instance succeeded."
    return instances


startNewWait :: EC2Resource m
             => Types.RunInstancesRequest -> [Tag] -> EC2 m [Types.Instance]
startNewWait instanceRequest tags = do
    instances <- startNew instanceRequest tags
    let instanceIDs = map Types.instanceId instances
    waitForStart instanceIDs def


startExisting :: EC2Resource m
              => [Instance.ID] -> [Tag] -> EC2 m ()
startExisting instanceIDs tags = do
    logger info "Starting existing instance..."
    _ <- EC2.startInstances instanceIDs
    Tag.tag tags instanceIDs
    logger info "Starting existing instance succeeded."


startExistingWait :: EC2Resource m
                  => [Instance.ID] -> [Tag] -> EC2 m [Types.Instance]
startExistingWait instanceIDs tags = do
    startExisting instanceIDs tags
    waitForStart instanceIDs def


waitForStart :: EC2Resource m
             => [Instance.ID] -> WaitTimes -> EC2 m [Types.Instance]
waitForStart instanceIDs =
    waitForState instanceIDs Types.InstanceStateRunning


waitForState :: EC2Resource m
             => [Instance.ID] -> Types.InstanceState -> WaitTimes -> EC2 m [Types.Instance]
waitForState instanceIDs state waitTimes = do
    logger info $ "Waiting for instance to change state to " ++ show state ++ ". Please wait."
    userInstances <- Loops.iterateUntil (all $ hasState state) $ do
        liftIO $ do Concurrent.threadDelay $ WaitTime.next waitTimes
                    putStr "."
                    IO.hFlush IO.stdout
        byIDs instanceIDs
    liftIO $ putStrLn ""
    logger info "Instance state successfully changed!"
    return userInstances


findInstances :: EC2Resource m => [Types.Filter] ->  EC2 m [Types.Instance]
findInstances filter' = do
    logger debug "Looking for instances..."
    filter resumable <$> describeInstances [] filter'


byID :: EC2Resource m => Instance.ID -> EC2 m Types.Instance
byID instanceID = do
    instances <- byIDs [instanceID]
    case instances of
        [inst] -> return inst
        _      -> fail "Something went wrong on Instance.byID"


byIDs :: EC2Resource m => [Instance.ID] -> EC2 m [Types.Instance]
byIDs instanceIDs = describeInstances instanceIDs []


describeInstances :: EC2Resource m => [Instance.ID] -> [Types.Filter] -> EC2 m [Types.Instance]
describeInstances instanceIDs filter' =
    concatMap Types.reservationInstanceSet <$> Util.list (EC2.describeInstances instanceIDs filter')
