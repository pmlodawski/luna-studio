---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

module Flowbox.AWS.User.Database.Database where

import           Control.Monad
import qualified Data.Maybe                 as Maybe
import           Data.String                (fromString)
import qualified Database.PostgreSQL.Simple as PSQL

import qualified Flowbox.AWS.User.Database.AddUser as AddUser
import qualified Flowbox.AWS.User.Database.GetUser as GetUser
import qualified Flowbox.AWS.User.Database.Schema  as Schema
import           Flowbox.AWS.User.User             (User)
import qualified Flowbox.AWS.User.User             as User
import           Flowbox.Prelude



type Database = PSQL.Connection

type Error = String


mk :: PSQL.ConnectInfo -> IO Database
mk = PSQL.connect


addUser :: PSQL.Connection -> User -> IO ()
addUser connection user = PSQL.withTransaction connection $ do
    exising <- getUser connection $ user ^. User.name
    if Maybe.isJust exising
        then fail "User already exists"
        else void $ PSQL.execute connection (fromString AddUser.query)
                        $ User.toDB user


getUser :: PSQL.Connection -> User.Name -> IO (Maybe User)
getUser connection userName = fmap User.fromDB . Maybe.listToMaybe <$> PSQL.query connection
    (fromString GetUser.query) (PSQL.Only userName)


create :: PSQL.Connection -> IO ()
create connection = void $ PSQL.execute connection (fromString Schema.query) ()
