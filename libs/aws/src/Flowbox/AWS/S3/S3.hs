---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Flowbox.AWS.S3.S3 (
    module S3,

    S3Env(..),
    S3,
    runS3,
    runS3env,
    query,
    withBucket,
) where

import qualified Aws                          as Aws
import           Aws.S3                       as S3
import           Control.Monad.Reader
import qualified Control.Monad.Trans.Resource as Resource
import qualified Network.HTTP.Conduit         as Conduit

import Flowbox.Prelude



data S3Env = S3Env { config   :: Aws.Configuration
                   , s3config :: S3.S3Configuration Aws.NormalQuery
                   , manager  :: Conduit.Manager
                   , bucket   :: S3.Bucket
                   }

makeLenses (''S3Env)


type S3 a = ReaderT S3Env (Resource.ResourceT IO) a


runS3 :: Aws.Configuration -> S3.Bucket -> S3 a -> IO a
runS3 cfg b f = Conduit.withManager $ \mgr -> do
    let s3env = S3Env cfg Aws.defServiceConfig mgr b
    runReaderT f s3env


runS3env :: S3Env -> S3 a -> IO a
runS3env env f = Resource.runResourceT $ runReaderT f env

query :: (Aws.Transaction r b, Aws.ServiceConfiguration r ~ S3Configuration)
      => r -> S3 b
query request = do
    env <- ask
    lift $ Aws.pureAws (config env) (s3config env) (manager env) request


withBucket :: (S3.Bucket -> S3 a) -> S3 a
withBucket = ((bucket <$> ask) >>=)

