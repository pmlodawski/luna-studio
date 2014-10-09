---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}

module Flowbox.AccountManager.Handler.User where

import           Flowbox.AccountManager.Context                      (Context)
import qualified Flowbox.AccountManager.Context                      as Context
import qualified Flowbox.AWS.AccountManager                          as AccountManager
import           Flowbox.Control.Error
import           Flowbox.Prelude                                     hiding (Context, error)
import           Flowbox.System.Log.Logger
import           Flowbox.Tools.Serialize.Proto.Conversion.Basic
import           Flowbox.ZMQ.RPC.RPC                                 (RPC)
import qualified Generated.Proto.AccountManager.User.Login.Args      as User_Login
import qualified Generated.Proto.AccountManager.User.Login.Result    as User_Login
import qualified Generated.Proto.AccountManager.User.Logout.Args     as User_Logout
import qualified Generated.Proto.AccountManager.User.Logout.Result   as User_Logout
import qualified Generated.Proto.AccountManager.User.Register.Args   as User_Register
import qualified Generated.Proto.AccountManager.User.Register.Result as User_Register
import qualified Generated.Proto.AccountManager.User.Session.Args    as User_Session
import qualified Generated.Proto.AccountManager.User.Session.Result  as User_Session



logger :: LoggerIO
logger = getLoggerIO $(moduleName)

-------- public api -------------------------------------------------

register :: Context -> User_Register.Args -> RPC User_Register.Result
register ctx (User_Register.Args tuserName tpassword) = do
    logger info "called User::register"
    let userName = decodeP tuserName
        password = decodeP tpassword
    AccountManager.register (ctx ^. Context.database) userName password
    return User_Register.Result


login :: Context -> User_Login.Args -> RPC User_Login.Result
login ctx (User_Login.Args tuserName tpassword) = do
    logger info "called User::login"
    let userName = decodeP tuserName
        password = decodeP tpassword
    (ip, instanceID) <- AccountManager.login (ctx ^. Context.database)
                                             (ctx ^. Context.credential)
                                             (ctx ^. Context.region)
                                             userName password
    return $ User_Login.Result (encodeP $ show ip) (encodeP instanceID)


logout :: Context -> User_Logout.Args -> RPC User_Logout.Result
logout ctx (User_Logout.Args tuserName tpassword) = do
    logger info "called User::logout"
    let userName = decodeP tuserName
        password = decodeP tpassword
    AccountManager.logout (ctx ^. Context.database) userName password
    return $ User_Logout.Result


session :: Context -> User_Session.Args -> RPC User_Session.Result
session ctx (User_Session.Args tuserName tinstanceID) = do
    logger info "called User::session"
    let userName   = decodeP tuserName
        instanceID = decodeP tinstanceID
    sess <- safeLiftIO $ AccountManager.session (ctx ^. Context.database) userName instanceID
    return $ case sess of
        Nothing -> User_Session.Result False Nothing
        Just s  -> User_Session.Result True $ encodeJ s

