---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import qualified AWS                          as AWS
import           AWS.EC2                      (EC2)
import qualified AWS.EC2                      as EC2
import qualified AWS.EC2.Types                as Types
import qualified AWS.EC2.Util                 as Util
import qualified Control.Concurrent           as Concurrent
import           Control.Monad                (forM)
import           Control.Monad.IO.Class       (MonadIO, liftIO)
import qualified Control.Monad.Loops          as Loops
import qualified Control.Monad.Trans.Resource as Resource
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import           Text.Show.Pretty             (ppShow)

import qualified Flowbox.AWS.Instance as Instance
import           Flowbox.Prelude



type Tag = Text
type UserName = Text
type InstanceID = Text


region = Text.pack "eu-west-1"


main :: IO ()
main = do
    credential <- AWS.loadCredential
    let runEC2   = Resource.runResourceT . EC2.runEC2 credential
        userName = "zenon"
    ip <- runEC2 $ do EC2.setRegion region
                      Types.instanceIpAddress <$> Instance.get userName Instance.defaultInstanceRequest
    print ip
    putStrLn "quiting"
