---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}

module Flowbox.AWS.Session where

import           AWS.EC2              (EC2)
import qualified AWS.EC2              as EC2
import qualified AWS.EC2.Types        as Types
import           Data.ByteString.Lazy (ByteString)
import qualified Data.Digest.Pure.SHA as SHA
import qualified Data.Map             as Map

import           Data.IP                   (IPv4)
import qualified Flowbox.AWS.Instance      as Instance
import           Flowbox.AWS.User          (UserData (UserData), UserDatabase, UserName)
import           Flowbox.Prelude
import           Flowbox.System.Log.Logger



logger :: LoggerIO
logger = getLoggerIO "Flowbox.AWS.Session"


type Error = String


register :: UserName -> ByteString -> UserDatabase -> Either Error UserDatabase
register userName password' database = case Map.lookup userName database of
    Just _  -> Left "Cannot register user: username already exists"
    Nothing -> do let userData = UserData $ SHA.sha256 password'
                  Right $ Map.insert userName userData database


login :: Instance.EC2Resource m
      => UserName -> ByteString -> UserDatabase -> EC2 m (Either Error IPv4)
login userName password' database = case Map.lookup userName database of
    Nothing              -> return $ Left "Login failed: no such user"
    Just (UserData hash) -> if hash /= SHA.sha256 password'
                                then return $ Left "Login failed: password incorrect"
                                else do logger info $ "Login successful, username=" ++ (show userName)
                                        inst <- Instance.get userName Instance.defaultInstanceRequest
                                        Right <$> (fromJust $ Types.instanceIpAddress inst)


logout :: Instance.EC2Resource m => UserName -> EC2 m ()
logout userName = do
    logger info $ "Logout, terminating instances..."
    userInstances <- Instance.find userName
    _ <- EC2.terminateInstances $ map Types.instanceId userInstances
    logger info $ "Logout successful, username=" ++ (show userName)
    return ()

