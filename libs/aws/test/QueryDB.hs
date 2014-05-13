---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Main where

import qualified AWS                        as AWS
import qualified Aws                        as Aws
import qualified AWS.EC2.Types              as Types
import           Control.Monad.Reader
import           Data.Text                  (Text)
import           Database.PostgreSQL.Simple as PSQL

import qualified Flowbox.AWS.User.Database           as Database
import qualified Flowbox.AWS.User.Password           as Password
import           Flowbox.AWS.User.User               (User (User))
import           Flowbox.Control.Error
import           Flowbox.Prelude
import qualified Flowbox.System.Console.ASCIISpinner as Spinner



main :: IO (Either String ())
main = runEitherT $ do
    putStr "Connecting "
    db <- liftIO $ Spinner.runWithSpinner $ Database.mk
            $ PSQL.ConnectInfo "mydbinstance.cn1bxyb5bfdl.eu-west-1.rds.amazonaws.com"
                               5432
                               "test"
                               "kozatest123"
                               "flowbox"
    --Database.create db
    Database.addUser db $ User "stefan" $ Password.mk "ala123"
    Database.addUser db $ User "zenon"  $ Password.mk "ala123"
    u1 <- liftIO $ Database.getUser db "stefan"
    u2 <- liftIO $ Database.getUser db "zenon"
    print u1
    print u2

