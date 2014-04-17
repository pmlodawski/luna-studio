---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.AccountManager.Handler.User where

import qualified Data.IORef as IORef

import           Control.Monad.Trans.Either
import           Flowbox.AccountManager.Context                      (ContextRef)
import qualified Flowbox.AccountManager.Context                      as Context
import qualified Flowbox.AWS.EC2                                     as EC2
import qualified Flowbox.AWS.Session                                 as Session
import           Flowbox.Prelude                                     hiding (error)
import           Flowbox.System.Log.Logger
import           Flowbox.Tools.Serialize.Proto.Conversion.Basic
import           Flowbox.ZMQ.RPC.RPC                                 (RPC, liftIO)
import qualified Generated.Proto.AccountManager.User.Login.Args      as User_Login
import qualified Generated.Proto.AccountManager.User.Login.Result    as User_Login
import qualified Generated.Proto.AccountManager.User.Logout.Args     as User_Logout
import qualified Generated.Proto.AccountManager.User.Logout.Result   as User_Logout
import qualified Generated.Proto.AccountManager.User.Register.Args   as User_Register
import qualified Generated.Proto.AccountManager.User.Register.Result as User_Register



logger :: LoggerIO
logger = getLoggerIO "Flowbox.AccountManager.Handler.User"

-------- public api -------------------------------------------------

register :: ContextRef -> User_Register.Args -> RPC User_Register.Result
register ctxref (User_Register.Args tuserName tpassword) = do
    logger info "called User::register"
    let userName = decodeP tuserName
        password = decodeP tpassword
    ctx <- liftIO $ IORef.readIORef ctxref
    newDB <- hoistEither $ Session.register userName password $ Context.database ctx
    let newCtx = ctx { Context.database = newDB }
    liftIO $ IORef.writeIORef ctxref newCtx
    return User_Register.Result


login :: ContextRef -> User_Login.Args -> RPC User_Login.Result
login ctxref (User_Login.Args tuserName tpassword) = do
    logger info "called User::login"
    let userName = decodeP tuserName
        password = decodeP tpassword
    ctx <- liftIO  $ IORef.readIORef ctxref
    ip  <- EitherT $ EC2.runEC2inRegion (Context.credential ctx) (Context.region ctx)
                   $ Session.login userName password $ Context.database ctx
    return $ User_Login.Result $ encodeP $ show ip


logout :: ContextRef -> User_Logout.Args -> RPC User_Logout.Result
logout ctxref (User_Logout.Args tuserName) = do
    logger info "called User::logout"
    let userName = decodeP tuserName
    ctx <- liftIO $ IORef.readIORef ctxref
    liftIO $ EC2.runEC2inRegion (Context.credential ctx) (Context.region ctx)
           $ Session.logout userName
    return $ User_Logout.Result
