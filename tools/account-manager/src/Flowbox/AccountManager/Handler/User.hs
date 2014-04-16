---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.AccountManager.Handler.User where

import qualified Data.IORef as IORef

import           Flowbox.AccountManager.Context                      (Context)
import qualified Flowbox.AccountManager.Context                      as Context
import qualified Flowbox.AWS.Session                                 as Session
import           Flowbox.Prelude                                     hiding (Context, error)
import           Flowbox.System.Log.Logger
import           Flowbox.Tools.Serialize.Proto.Conversion.Basic
import qualified Generated.Proto.AccountManager.User.Login.Args      as User_Login
import qualified Generated.Proto.AccountManager.User.Login.Result    as User_Login
import qualified Generated.Proto.AccountManager.User.Logout.Args     as User_Logout
import qualified Generated.Proto.AccountManager.User.Logout.Result   as User_Logout
import qualified Generated.Proto.AccountManager.User.Register.Args   as User_Register
import qualified Generated.Proto.AccountManager.User.Register.Result as User_Register


logger :: LoggerIO
logger = getLoggerIO "Flowbox.AccountManager.Handler.User"

-------- public api -------------------------------------------------

register :: Context -> User_Register.Args -> IO User_Register.Result
register ctxref (User_Register.Args tuserName tpassword) = do
    logger info "called User::register"
    --let userName = decodeP tuserName
    --    password = decodeP tpassword
    ctx <- IORef.readIORef ctxref
    logger error "not implemented"
    return User_Register.Result


login :: Context -> User_Login.Args -> IO User_Login.Result
login ctxref (User_Login.Args tuserName tpassword) = do
    logger info "called User::login"
    logger error "not implemented"
    return $ User_Login.Result undefined


logout :: Context -> User_Logout.Args -> IO User_Logout.Result
logout ctxref (User_Logout.Args tuserName) = do
    logger info "called User::logout"
    logger error "not implemented"
    return $ User_Logout.Result
