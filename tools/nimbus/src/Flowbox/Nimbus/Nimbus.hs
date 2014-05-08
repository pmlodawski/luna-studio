---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Flowbox.Nimbus.Nimbus where

import qualified AWS           as AWS
import qualified AWS.EC2.Types as Types
import qualified Data.Text     as Text

import qualified Flowbox.AWS.EC2.EC2               as EC2
import qualified Flowbox.AWS.EC2.Instance.Instance as Instance
import qualified Flowbox.AWS.EC2.Instance.Request  as Request
import           Flowbox.AWS.Region                (Region)
import qualified Flowbox.Nimbus.Cmd                as Cmd
import           Flowbox.Prelude
import           Flowbox.System.Log.Logger



logger :: LoggerIO
logger = getLoggerIO "Flowbox.Nimbus.Nimbus"


getCredential :: Cmd.Options -> IO AWS.Credential
getCredential = AWS.loadCredentialFromFile . Cmd.credentialPath


userName :: String
userName = "flowbox"


instanceIP :: Types.Instance -> String
instanceIP inst = case Types.instanceIpAddress inst of
    Just ip -> show ip
    Nothing -> "(no ip)"


printInstance :: Types.Instance -> IO ()
printInstance inst = putStrLn $ (Text.unpack $ Types.instanceId inst)
                             ++ " "
                             ++ instanceIP inst


start :: Region -> Cmd.Options -> IO ()
start region options = do
    credential <- getCredential options
    inst <- EC2.runEC2InRegion credential region $ do
        let request = Request.mk { Types.runInstancesRequestImageId      = Text.pack        $ Cmd.ami options
                                 , Types.runInstancesRequestInstanceType = Just $ Text.pack $ Cmd.machine options
                                 , Types.runInstancesRequestKeyName      = Just $ Text.pack $ Cmd.keyName options
                                 }
        Instance.getOrStart userName request
    let command = "ssh -i " ++ Cmd.keyName options ++ " ec2-user@" ++ instanceIP inst
    logger info $ "You can login using command " ++ show command
    printInstance inst


stop :: Region -> Cmd.Options -> IO ()
stop region options = do
    credential <- getCredential options
    EC2.runEC2InRegion credential region $ do
        instances <- filter Instance.ready <$> Instance.find userName
        let instanceIDs = map Types.instanceId instances
        if length instances == 0
            then logger info $ "No instances to stop."
            else do logger info $ "Stopping " ++ (show $ length instances) ++ " instances."
                    _ <- EC2.stopInstances instanceIDs $ Cmd.force options
                    return ()


get :: Region -> Cmd.Options -> IO ()
get region options = do
    credential <- getCredential options
    instances <- EC2.runEC2InRegion credential region $ Instance.find userName
    mapM_ printInstance $ filter Instance.ready instances
