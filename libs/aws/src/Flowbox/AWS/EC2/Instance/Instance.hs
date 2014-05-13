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

module Flowbox.AWS.EC2.Instance.Instance where

import qualified AWS.EC2.Types          as Types
import qualified AWS.EC2.Util           as Util
import qualified Control.Concurrent     as Concurrent
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Control.Monad.Loops    as Loops
import qualified System.IO              as IO

import           Flowbox.AWS.EC2.EC2               (EC2, EC2Resource)
import qualified Flowbox.AWS.EC2.EC2               as EC2
import qualified Flowbox.AWS.EC2.Instance.ID       as Instance
import qualified Flowbox.AWS.EC2.Instance.Tag      as Tag
import           Flowbox.AWS.EC2.Instance.WaitTime (WaitTimes)
import qualified Flowbox.AWS.EC2.Instance.WaitTime as WaitTime
import           Flowbox.AWS.Tag                   (Tag)
import           Flowbox.Prelude
import           Flowbox.System.Log.Logger



logger :: LoggerIO
logger = getLoggerIO "Flowbox.AWS.EC2.Instance.Instance"


ready :: Types.Instance -> Bool
ready inst = Types.instanceState inst == Types.InstanceStateRunning


resumable :: Types.Instance -> Bool
resumable inst = Types.instanceState inst == Types.InstanceStateRunning
              || Types.instanceState inst == Types.InstanceStateStopped
              || Types.instanceState inst == Types.InstanceStateStopping


startNew :: EC2Resource m
         => Types.RunInstancesRequest -> [Tag] -> EC2 m Types.Instance
startNew instanceRequest tags = do
    logger info "Starting new instance..."
    reservation <- EC2.runInstances instanceRequest
    let instanceIDs = map Types.instanceId $ Types.reservationInstanceSet reservation
    Tag.tag tags instanceIDs
    logger info "Starting new instance succeeded."
    [userInstance] <- waitForStart instanceIDs def
    return userInstance


startExisting :: EC2Resource m
              => Instance.ID -> [Tag] -> EC2 m Types.Instance
startExisting instanceID tags = do
    logger info "Starting existing instance..."
    _ <- EC2.startInstances [instanceID]
    Tag.tag tags [instanceID]
    logger info "Starting existing instance succeeded."
    userInstances <- waitForStart [instanceID] def
    case userInstances of
        [userInstance] -> return userInstance
        _              -> fail "Something wrong happened : multiple instances started"


waitForStart :: EC2Resource m
             => [Instance.ID] -> WaitTimes -> EC2 m [Types.Instance]
waitForStart instanceIDs waitTimes = do
    logger info "Waiting for instance start. Please wait."
    liftIO $ Concurrent.threadDelay $ WaitTime.initial waitTimes
    userInstances <- Loops.iterateUntil (all ready) $ do
        userInstances <- concatMap Types.reservationInstanceSet <$> (Util.list $ EC2.describeInstances instanceIDs [])
        liftIO $ do putStr "."
                    IO.hFlush IO.stdout
                    Concurrent.threadDelay $ WaitTime.next waitTimes
        return userInstances
    liftIO $ putStrLn ""
    logger info "Instance is ready!"
    return userInstances


findInstances :: EC2Resource m => [Types.Filter] ->  EC2 m [Types.Instance]
findInstances filter' = do
    logger debug "Looking for instances..."
    filter resumable <$> concatMap Types.reservationInstanceSet <$> (Util.list $ EC2.describeInstances [] filter')


byID :: EC2Resource m => Instance.ID -> EC2 m Types.Instance
byID instanceID = do
    instances <- concatMap Types.reservationInstanceSet <$> (Util.list $ EC2.describeInstances [instanceID] [])
    case instances of
        [inst] -> return inst
        _      -> fail "Something went wrong on Instance.byID"

