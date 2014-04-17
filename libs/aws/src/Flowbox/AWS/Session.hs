---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}

module Flowbox.AWS.Session where

import           AWS.EC2       (EC2)
import qualified AWS.EC2       as EC2
import qualified AWS.EC2.Types as Types

import           Data.IP                   (IPv4)
import qualified Flowbox.AWS.Instance      as Instance
import           Flowbox.AWS.User.Database (Database)
import qualified Flowbox.AWS.User.Database as Database
import qualified Flowbox.AWS.User.Password as Password
import qualified Flowbox.AWS.User.User     as User
import           Flowbox.Prelude
import           Flowbox.System.Log.Logger



logger :: LoggerIO
logger = getLoggerIO "Flowbox.AWS.Session"


type Error = String


register :: User.Name -> Password.Plain -> Database -> Either Error Database
register userName password database = case Database.lookup userName database of
    Just _  -> Left "Cannot register user: username already exists"
    Nothing -> do let userData = User.Data $ Password.mk password
                  Right $ Database.insert userName userData database


login :: Instance.EC2Resource m
      => User.Name -> Password.Plain -> Database -> EC2 m (Either Error IPv4)
login userName password database = case Database.lookup userName database of
    Nothing              -> return $ Left "Login failed: no such user"
    Just (User.Data hash) -> if hash /= Password.mk password
                                then return $ Left "Login failed: password incorrect"
                                else do logger info $ "Login successful, username=" ++ (show userName)
                                        inst <- Instance.get userName Instance.defaultInstanceRequest
                                        Right <$> (fromJust $ Types.instanceIpAddress inst)


logout :: Instance.EC2Resource m => User.Name -> EC2 m ()
logout userName = do
    logger info $ "Logout, terminating instances..."
    userInstances <- Instance.find userName
    _ <- EC2.terminateInstances $ map Types.instanceId userInstances
    logger info $ "Logout successful, username=" ++ (show userName)
    return ()

