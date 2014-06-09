---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Flowbox.FileManager.Context where

import qualified Aws
import qualified Network.HTTP.Conduit as Conduit

import qualified Flowbox.AWS.S3.S3 as S3
import           Flowbox.Prelude   hiding (Context)


type Context = S3.S3Env


mk :: Aws.Configuration -> S3.Bucket -> IO Context
mk cfg bucket = do
    manager <- Conduit.newManager Conduit.conduitManagerSettings
    return $ S3.S3Env cfg Aws.defServiceConfig manager bucket
