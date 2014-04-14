---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Main where

import Flowbox.Prelude

import qualified AWS                          as AWS
import qualified AWS.EC2                      as EC2
import qualified AWS.EC2.Types                as Types
import qualified AWS.EC2.Util                 as Util
import qualified Control.Concurrent           as Concurrent
import           Control.Monad                (forM)
import qualified Control.Monad.Trans.Resource as Resource
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import           Text.Show.Pretty             (ppShow)
import Control.Monad.IO.Class (liftIO)



type Tag = Text


main :: IO ()
main = do
    credential <- AWS.loadCredential
    let runEC2   = Resource.runResourceT . EC2.runEC2 credential
        region           = Text.pack "eu-west-1"
        userTagKey       = Text.pack "user"
        userName         = Text.pack "zenon"
        imageID          = Text.pack "ami-a921dfde"
        instanceType     = Text.pack "t1.micro"
        requestSecurityGroups = [Text.pack "launch-wizard-1"]
    runEC2 $ do EC2.setRegion region
                let userFilter = [(Text.append (Text.pack "tag:") userTagKey, [userName])]
                userInstances <- concatMap Types.reservationInstanceSet <$> (Util.list $ EC2.describeInstances [] userFilter)
                putStrLn $ ppShow userInstances
                let instanceRequest = Types.RunInstancesRequest imageID -- ImageId :: Text
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
                putStrLn "==== Run instances ===="
                reservation <- EC2.runInstances instanceRequest
                let resourceIDs = map Types.instanceId $ Types.reservationInstanceSet reservation
                True <- EC2.createTags resourceIDs [(userTagKey, userName)]
                putStrLn $ ppShow reservation
                putStrLn "==== Run instances succeeded ===="
                forM [0..] $ \i -> do putStrLn $ "==== Retry " ++ (show i) ++ " ===="
                                      liftIO $ Concurrent.threadDelay 5000000
                                      userInstances <- concatMap Types.reservationInstanceSet <$> (Util.list $ EC2.describeInstances [] userFilter)
                                      putStrLn $ ppShow userInstances

    putStrLn "quiting"
