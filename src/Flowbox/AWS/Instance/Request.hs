---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.AWS.Instance.Request where

import qualified AWS.EC2.Types as Types
import           Data.Text     (Text)
import qualified Data.Text     as Text

import Flowbox.Prelude



imageID :: Text
imageID = Text.pack "ami-a921dfde"

instanceType :: Text
instanceType = Text.pack "t1.micro"

requestSecurityGroups :: [Text]
requestSecurityGroups = [Text.pack "launch-wizard-1"]


mk :: Types.RunInstancesRequest
mk = Types.RunInstancesRequest
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
