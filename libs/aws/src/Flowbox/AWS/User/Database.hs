---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

module Flowbox.AWS.User.Database where

import           Control.Monad
import qualified Database.PostgreSQL.Simple as PSQL

import           Flowbox.AWS.User.User (User)
import qualified Flowbox.AWS.User.User as User
import           Flowbox.Control.Error
import           Flowbox.Prelude



type Database = PSQL.Connection

type Error = String


mk :: PSQL.ConnectInfo -> IO Database
mk = PSQL.connect


addUser :: PSQL.Connection -> User -> EitherT Error IO ()
addUser connection user = safeLiftIO $ PSQL.withTransaction connection $ do
    exising <- getUser connection $ User.name user
    if null exising
        then void $ PSQL.execute connection
                        "insert into Users (UserName, Password) values (?, ?)"
                        $ User.toDB user
        else fail "User already exists"


getUser :: PSQL.Connection -> User.Name -> IO [User]
getUser connection userName = map User.fromDB <$> PSQL.query connection
    "select * from Users where Username=?" (PSQL.Only userName)


create :: PSQL.Connection -> IO ()
create connection = void $ PSQL.execute connection
    "create table Users ( UserName char(20) PRIMARY KEY, Password char(40) NOT NULL )" ()
