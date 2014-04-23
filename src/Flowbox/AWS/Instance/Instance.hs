---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Flowbox.AWS.Instance.Instance where

import           AWS.EC2                      (EC2)
import qualified AWS.EC2                      as EC2
import qualified AWS.EC2.Types                as Types
import qualified AWS.EC2.Util                 as Util
import qualified Control.Concurrent           as Concurrent
import           Control.Monad.IO.Class       (MonadIO, liftIO)
import qualified Control.Monad.Loops          as Loops
import qualified Control.Monad.Trans.Resource as Resource
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import qualified System.IO                    as IO

import           Flowbox.AWS.Instance.WaitTime (WaitTimes)
import qualified Flowbox.AWS.Instance.WaitTime as WaitTime
import qualified Flowbox.AWS.User.User         as User
import           Flowbox.Prelude
import           Flowbox.System.Log.Logger


logger :: LoggerIO
logger = getLoggerIO "Flowbox.AWS.Instance"


type InstanceID = Text

type EC2Resource m = (MonadIO m, Resource.MonadResource m, Resource.MonadBaseControl IO m)


userTagKey :: Text
userTagKey = Text.pack "user"


find :: EC2Resource m
     => User.Name -> EC2 m [Types.Instance]
find userName = do
    logger debug "Looking for instances..."
    let userFilter = [(Text.append (Text.pack "tag:") userTagKey, [Text.pack userName])]
    concatMap Types.reservationInstanceSet <$> (Util.list $ EC2.describeInstances [] userFilter)


startNew :: EC2Resource m
         => User.Name -> Types.RunInstancesRequest -> EC2 m Types.Instance
startNew userName instanceRequest = do
    logger info "Starting new instance..."
    reservation <- EC2.runInstances instanceRequest
    let instanceIDs = map Types.instanceId $ Types.reservationInstanceSet reservation
    True <- EC2.createTags instanceIDs [(userTagKey, Text.pack userName)]
    logger info "Starting new instance succeeded."
    [userInstance] <- waitForStart instanceIDs def
    return userInstance


startExisting :: EC2Resource m
              => InstanceID -> EC2 m Types.Instance
startExisting instanceID = do
    logger info "Starting existing instance..."
    _ <- EC2.startInstances [instanceID]
    logger info "Starting existing succeeded."
    [userInstance] <- waitForStart [instanceID] def
    return userInstance


ready :: Types.Instance -> Bool
ready inst = Types.InstanceStateRunning == Types.instanceState inst


waitForStart :: EC2Resource m
             => [InstanceID] -> WaitTimes -> EC2 m [Types.Instance]
waitForStart instanceIDs waitTimes = do
    logger info "Waiting for instance start. Please wait."
    liftIO $ Concurrent.threadDelay $ WaitTime.initial waitTimes
    userInstances <- Loops.iterateUntil (all ready) $ do
        userInstances <- concatMap Types.reservationInstanceSet <$> (Util.list $ EC2.describeInstances instanceIDs [])
        liftIO $ putStr "."
        liftIO $ IO.hFlush IO.stdout
        liftIO $ Concurrent.threadDelay $ WaitTime.next waitTimes
        return userInstances
    liftIO $ putStrLn ""
    logger info "Instance is ready!"
    return userInstances


getOrStart :: EC2Resource m
           => User.Name -> Types.RunInstancesRequest -> EC2 m Types.Instance
getOrStart userName instanceRequest = do
    let usable inst = state /= Types.InstanceStateTerminated && state /= Types.InstanceStateShuttingDown where state = Types.instanceState inst
    userInstances <- filter usable <$> find userName
    case map Types.instanceState userInstances of
            []                           -> startNew userName instanceRequest
            [Types.InstanceStatePending] -> head <$> waitForStart (map Types.instanceId userInstances) def
            [Types.InstanceStateRunning] -> return $ head userInstances
            [Types.InstanceStateStopped] -> startExisting $ Types.instanceId $ head userInstances
            [_]                          -> startNew userName instanceRequest
            _                            -> undefined

