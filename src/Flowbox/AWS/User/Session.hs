---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}

module Flowbox.AWS.User.Session where

import           AWS.EC2                (EC2)
import qualified AWS.EC2                as EC2
import qualified AWS.EC2.Types          as Types
import           Control.Monad.IO.Class

import           Data.IP                       (IPv4)
import qualified Flowbox.AWS.Instance.Instance as Instance
import qualified Flowbox.AWS.Instance.Request  as Request
import           Flowbox.AWS.User.Database     (Database)
import qualified Flowbox.AWS.User.Database     as Database
import qualified Flowbox.AWS.User.Password     as Password
import           Flowbox.AWS.User.User         (User (User))
import qualified Flowbox.AWS.User.User         as User
import           Flowbox.Prelude
import           Flowbox.System.Log.Logger


logger :: LoggerIO
logger = getLoggerIO "Flowbox.AWS.Session"


type Error = String


register :: User.Name -> Password.Plain -> Database -> IO (Either Error ())
register userName password database = do
    Database.addUser database $ User userName $ Password.mk password


login :: Instance.EC2Resource m
      => User.Name -> Password.Plain -> Database -> EC2 m (Either Error IPv4)
login userName password database = do
    users <- liftIO $ Database.getUser database userName
    case users of
        [User _ hash] -> if Password.verify hash password
                            then do logger info $ "Login successful, username=" ++ (show userName)
                                    inst <- Instance.getOrStart userName Request.mk
                                    Right <$> (fromJust $ Types.instanceIpAddress inst)
                            else return $ Left "Login failed."
        _             -> return $ Left "Login failed."


logout :: Instance.EC2Resource m => User.Name -> EC2 m ()
logout userName = do
    logger info $ "Logout, terminating instances..."
    userInstances <- Instance.find userName
    _ <- EC2.terminateInstances $ map Types.instanceId userInstances
    logger info $ "Logout successful, username=" ++ (show userName)
    return ()

