---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Flowbox.InstanceManager.InstanceManager where

import qualified AWS           as AWS
import qualified AWS.EC2.Types as Types
import qualified Data.Text     as Text

import qualified Flowbox.AWS.EC2               as EC2
import qualified Flowbox.AWS.Instance.Instance as Instance
import qualified Flowbox.AWS.Instance.Request  as Request
import           Flowbox.AWS.Region            (Region)
import qualified Flowbox.InstanceManager.Cmd   as Cmd
import           Flowbox.Prelude



getCredential :: Cmd.Options -> IO AWS.Credential
getCredential = AWS.loadCredentialFromFile . Cmd.credentialPath


userName :: String
userName = "flowbox"


printInstance :: Types.Instance -> IO ()
printInstance inst = putStrLn $ (Text.unpack $ Types.instanceId inst)
                             ++ " "
                             ++ (case Types.instanceIpAddress inst of
                                    Just ip -> show ip
                                    Nothing -> "(no ip)")


start :: Region -> Cmd.Options -> IO ()
start region options = do
    credential <- getCredential options
    inst <- EC2.runEC2InRegion credential region $ do
        let request = Request.mk { Types.runInstancesRequestImageId      = Text.pack        $ Cmd.ami options
                                 , Types.runInstancesRequestInstanceType = Just $ Text.pack $ Cmd.machine options
                                 }
        Instance.get userName request
    printInstance inst


stop :: Region -> Cmd.Options -> IO ()
stop region options = do
    credential <- getCredential options
    let instanceID = Text.pack $ Cmd.instanceID options
    _ <- EC2.runEC2InRegion credential region $ EC2.stopInstances [instanceID] $ Cmd.force options
    return ()


get :: Region -> Cmd.Options -> IO ()
get region options = do
    credential <- getCredential options
    instances <- EC2.runEC2InRegion credential region $ Instance.find userName
    mapM_ printInstance $ filter Instance.ready instances
