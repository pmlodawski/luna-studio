---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.AccountManager.Handler.User where

import           Flowbox.AccountManager.Context                      (Context)
import qualified Flowbox.AccountManager.Context                      as Context
import qualified Flowbox.AWS.AccountManager                          as AccountManager
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



logger :: LoggerIO
logger = getLoggerIO "Flowbox.AccountManager.Handler.User"

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
    AccountManager.authenticate (ctx ^. Context.database) userName password
    return $ User_Login.Result $ encodeP $ "127.0.0.1"


logout :: Context -> User_Logout.Args -> RPC User_Logout.Result
logout ctx (User_Logout.Args tuserName tpassword) = do
    logger info "called User::logout"
    let userName = decodeP tuserName
        password = decodeP tpassword
    AccountManager.authenticate (ctx ^. Context.database) userName password
    return $ User_Logout.Result
