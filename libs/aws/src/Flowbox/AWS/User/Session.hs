---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}

module Flowbox.AWS.User.Session where

import           AWS.EC2                (EC2)
import qualified AWS.EC2                as EC2
import qualified AWS.EC2.Types          as Types
import           Control.Monad.IO.Class
import           Data.IP                (IPv4)

import qualified Flowbox.AWS.EC2.Instance.Instance as Instance
import qualified Flowbox.AWS.EC2.Instance.Request  as Request
import           Flowbox.AWS.User.Database         (Database)
import qualified Flowbox.AWS.User.Database         as Database
import qualified Flowbox.AWS.User.Password         as Password
import           Flowbox.AWS.User.User             (User (User))
import qualified Flowbox.AWS.User.User             as User
import           Flowbox.Control.Error
import           Flowbox.Prelude
import           Flowbox.System.Log.Logger



logger :: LoggerIO
logger = getLoggerIO "Flowbox.AWS.Session"


type Error = String


register :: User.Name -> Password.Plain -> Database -> EitherT Error IO ()
register userName password database =
    Database.addUser database $ User userName $ Password.mk password


authenticate :: User.Name -> Password.Plain -> Database -> EitherT Error IO ()
authenticate userName password database = do
    users <- safeLiftIO $ Database.getUser database userName
    case users of
        [User _ hash] -> if Password.verify hash password
                            then right ()
                            else left "Authentication failed"
        _             -> left "Authentication failed"

login :: Instance.EC2Resource m
      => User.Name -> Password.Plain -> Database -> EC2 m (Either Error IPv4)
login userName password database = do
    auth <- liftIO $ runEitherT $ authenticate userName password database
    case auth of
        Right () -> Right <$> start userName
        Left msg -> return $ Left msg


logout :: Instance.EC2Resource m => User.Name -> Password.Plain -> Database -> EC2 m (Either Error ())
logout userName password database = do
    auth <- liftIO $ runEitherT $ authenticate userName password database
    case auth of
        Right () -> Right <$> end userName
        Left msg -> return $ Left msg


start :: Instance.EC2Resource m => User.Name -> EC2 m IPv4
start userName = do
    logger info $ "Starting session for, username=" ++ (show userName)
    inst <- Instance.getOrStart userName Request.mk
    fromJust $ Types.instanceIpAddress inst


end :: Instance.EC2Resource m => User.Name -> EC2 m ()
end userName = do
    logger info $ "Ending session for, username=" ++ (show userName)
    userInstances <- Instance.find userName
    _ <- EC2.terminateInstances $ map Types.instanceId userInstances
    return ()
