---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE MultiParamTypeClasses #-}

module Flowbox.FileManager.Context where

import qualified Aws
import           Control.Monad.Trans.State
import qualified Network.HTTP.Conduit      as Conduit

import           Flowbox.AWS.S3.S3     (S3)
import qualified Flowbox.AWS.S3.S3     as S3
import           Flowbox.Bus.RPC.RPC   (RPC)
import           Flowbox.Control.Error
import           Flowbox.Prelude       hiding (Context)



type Context = S3.S3Env


mk :: Aws.Configuration -> S3.Bucket -> IO Context
mk cfg bucket = do
    manager <- Conduit.newManager Conduit.conduitManagerSettings
    return $ S3.S3Env cfg Aws.defServiceConfig manager bucket


run :: S3 a -> RPC Context IO a
run s3 = do
    ctx <- lift $ get
    safeLiftIO $ S3.runS3env ctx s3
