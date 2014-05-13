---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Flowbox.AccountManager.Handler.User where

import Control.Monad.IO.Class     (liftIO)
import Control.Monad.Trans.Either

import           Flowbox.AccountManager.Context                      (Context)
import qualified Flowbox.AccountManager.Context                      as Context
import qualified Flowbox.AWS.EC2.EC2                                 as EC2
import qualified Flowbox.AWS.User.Session                            as Session
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
    Session.register userName password $ Context.database ctx
    return User_Register.Result


login :: Context -> User_Login.Args -> RPC User_Login.Result
login ctx (User_Login.Args tuserName tpassword) = do
    logger info "called User::login"
    let userName = decodeP tuserName
        password = decodeP tpassword
    ip  <- EitherT $ EC2.runEC2InRegion (Context.credential ctx) (Context.region ctx)
                   $ Session.login userName password (Context.pool ctx) (Context.database ctx)
    return $ User_Login.Result $ encodeP $ show ip


logout :: Context -> User_Logout.Args -> RPC User_Logout.Result
logout ctx (User_Logout.Args tuserName tpassword) = do
    logger info "called User::logout"
    let userName = decodeP tuserName
        password = decodeP tpassword
    EitherT $ liftIO $ EC2.runEC2InRegion (Context.credential ctx) (Context.region ctx)
                     $ Session.logout userName password (Context.pool ctx) (Context.database ctx)
    return $ User_Logout.Result
