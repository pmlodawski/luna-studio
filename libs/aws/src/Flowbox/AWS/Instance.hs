---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Flowbox.AWS.Instance where

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

import Flowbox.AWS.User          (UserName)
import Flowbox.Prelude
import Flowbox.System.Log.Logger



logger :: LoggerIO
logger = getLoggerIO "Flowbox.AWS.Instance"


type InstanceID = Text
type Region     = Text
type ImageId    = Text
type Type       = Text

type EC2Resource m = (MonadIO m, Resource.MonadResource m, Resource.MonadBaseControl IO m)


userTagKey :: Text
userTagKey = Text.pack "user"

imageID :: ImageId
imageID = Text.pack "ami-a921dfde"

instanceType :: Type
instanceType = Text.pack "t1.micro"

requestSecurityGroups :: [Text]
requestSecurityGroups = [Text.pack "launch-wizard-1"]


defaultInstanceRequest :: Types.RunInstancesRequest
defaultInstanceRequest = Types.RunInstancesRequest
    imageID -- ImageId :: Text
    1 -- MinCount :: Int
    1 -- MaxCount :: Int
    Nothing -- KeyName :: Maybe Text
    [] -- SecurityGroupIds :: [Text]
    requestSecurityGroups -- SecurityGroups :: [Text]
    Nothing -- UserData :: Maybe ByteString
    (Just instanceType) -- InstanceType :: Maybe Text
    Nothing -- AvailabilityZone :: Maybe Text
    Nothing -- PlacementGroup :: Maybe Text
    Nothing -- Tenancy :: Maybe Text
    Nothing -- KernelId :: Maybe Text
    Nothing -- RamdiskId :: Maybe Text
    [] -- BlockDeviceMappings :: [BlockDeviceMappingParam]
    Nothing -- MonitoringEnabled :: Maybe Bool
    Nothing -- SubnetId :: Maybe Text
    Nothing -- DisableApiTermination :: Maybe Bool
    Nothing -- ShutdownBehavior :: Maybe ShutdownBehavior
    Nothing -- PrivateIpAddress :: Maybe IPv4
    Nothing -- ClientToken :: Maybe Text
    [] -- NetworkInterfaces :: [NetworkInterfaceParam]
    Nothing -- IamInstanceProfile :: Maybe IamInstanceProfile
    Nothing -- runInstancesRequestEbsOptimized :: Maybe Bool


data WaitTimes = WaitTimes { initialWaitTime :: Int
                           , nextWaitTime    :: Int
                           , repeatCount     :: Int
                           } deriving (Show, Read, Eq, Ord)


defaultWaitTime :: WaitTimes
defaultWaitTime = WaitTimes 10000000 10000000 100


find :: EC2Resource m
     => UserName -> EC2 m [Types.Instance]
find userName = do
    let userFilter = [(Text.append (Text.pack "tag:") userTagKey, [Text.pack userName])]
    concatMap Types.reservationInstanceSet <$> (Util.list $ EC2.describeInstances [] userFilter)


startNew :: EC2Resource m
         => UserName -> Types.RunInstancesRequest -> EC2 m Types.Instance
startNew userName instanceRequest = do
    logger info "Starting new instance..."
    reservation <- EC2.runInstances instanceRequest
    let instanceIDs = map Types.instanceId $ Types.reservationInstanceSet reservation
    True <- EC2.createTags instanceIDs [(userTagKey, Text.pack userName)]
    logger info "Starting new instance succeeded."
    [userInstance] <- waitForStart instanceIDs defaultWaitTime
    return userInstance


startExisting :: EC2Resource m
              => InstanceID -> EC2 m Types.Instance
startExisting instanceID = do
    logger info "Starting existing instance..."
    _ <- EC2.startInstances [instanceID]
    logger info "Starting existing succeeded."
    [userInstance] <- waitForStart [instanceID] defaultWaitTime
    return userInstance


ready :: Types.Instance -> Bool
ready inst = Types.InstanceStateRunning == Types.instanceState inst


waitForStart :: EC2Resource m
             => [InstanceID] -> WaitTimes -> EC2 m [Types.Instance]
waitForStart instanceIDs waitTimes = do
    logger info "Waiting for instance start"
    liftIO $ Concurrent.threadDelay $ initialWaitTime waitTimes
    userInstances <- Loops.iterateUntil (all ready) $ do
        userInstances <- concatMap Types.reservationInstanceSet <$> (Util.list $ EC2.describeInstances instanceIDs [])
        logger info "Still waiting for instance start"
        liftIO $ Concurrent.threadDelay $ nextWaitTime waitTimes
        return userInstances
    logger info "Instance is ready!"
    return userInstances


get :: EC2Resource m
    => UserName -> Types.RunInstancesRequest -> EC2 m Types.Instance
get userName instanceRequest = do
    userInstances <- find userName
    case map Types.instanceState userInstances of
            []                           -> startNew userName instanceRequest
            [Types.InstanceStatePending] -> head <$> waitForStart (map Types.instanceId userInstances) defaultWaitTime
            [Types.InstanceStateRunning] -> return $ head userInstances
            [Types.InstanceStateStopped] -> startExisting $ Types.instanceId $ head userInstances
            [_]                          -> startNew userName instanceRequest
            _                            -> undefined

