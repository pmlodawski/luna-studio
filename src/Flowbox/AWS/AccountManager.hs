---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}

module Flowbox.AWS.AccountManager where

import qualified AWS
import qualified AWS.EC2.Types as Types
import           Data.IP       (IPv4)

import           Flowbox.AWS.Database.Database           (Database)
import qualified Flowbox.AWS.Database.User               as DBUser
import qualified Flowbox.AWS.EC2.Control.DBPool.Instance as Instance
import qualified Flowbox.AWS.EC2.Instance.Request        as Request
import           Flowbox.AWS.Region                      (Region)
import qualified Flowbox.AWS.User.Password               as Password
import           Flowbox.AWS.User.User                   (User (User))
import qualified Flowbox.AWS.User.User                   as User
import           Flowbox.Control.Error
import           Flowbox.Prelude
import           Flowbox.System.Log.Logger



logger :: LoggerIO
logger = getLoggerIO "Flowbox.AWS.AccountManager"


type Error = String


register :: Database -> User.Name -> Password.Plain -> EitherT Error IO ()
register database userName' plain = safeLiftIO $
    DBUser.add database $ User userName' (Password.mk salt plain) credit
    where
        --FIXME [PM] : salt!
        salt = "000"
        --FIXME [PM] : credit!
        credit = 10000


authenticate :: Database -> User.Name -> Password.Plain -> EitherT Error IO ()
authenticate database userName plain = do
    users <- safeLiftIO $ DBUser.find database userName
    case users of
        Just user -> if Password.verify (user ^. User.password) plain
                            then right ()
                            else left "Authentication failed"
        _         -> left "Authentication failed"


login :: Database -> AWS.Credential -> Region -> User.Name -> Password.Plain -> EitherT Error IO IPv4
login database credential region userName plain = do
    authenticate database userName plain
    safeLiftIO $ start database credential region userName


logout :: Database -> User.Name -> Password.Plain -> EitherT Error IO ()
logout database userName plain  = do
    authenticate database userName plain
    safeLiftIO $ end database userName


start :: Database -> AWS.Credential -> Region -> User.Name -> IO IPv4
start database credential region userName = do
    logger info $ "Starting session for, username=" ++ show userName
    [inst] <- Instance.retrieve database credential region userName Request.mk
    fromJust $ Types.instanceIpAddress inst


end :: Database -> User.Name -> IO ()
end database userName = do
    logger info $ "Ending session for, username=" ++ show userName
    Instance.releaseUser database userName
