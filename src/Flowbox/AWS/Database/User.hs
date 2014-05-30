---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.AWS.Database.User where

import           Control.Monad
import qualified Data.Maybe                 as Maybe
import           Data.String                (fromString)
import qualified Database.PostgreSQL.Simple as PSQL

import qualified Flowbox.AWS.Database.SQL.User.Add  as UserAdd
import qualified Flowbox.AWS.Database.SQL.User.Find as UserFind
import           Flowbox.AWS.User.Password          (Password (Password))
import qualified Flowbox.AWS.User.Password          as Password
import           Flowbox.AWS.User.User              (User (User))
import qualified Flowbox.AWS.User.User              as User
import           Flowbox.Prelude



add :: PSQL.Connection -> User -> IO ()
add connection user =
    void $ PSQL.execute connection (fromString UserAdd.query) $ toDB user


find :: PSQL.Connection -> User.Name -> IO (Maybe User)
find connection userName = fmap fromDB . Maybe.listToMaybe
    <$> PSQL.query connection (fromString UserFind.query) (PSQL.Only userName)


fromDB :: (String, String, String, Int) -> User
fromDB (name, salt, hash, credit) =
    User name (Password salt hash) credit


toDB :: User -> (String, String, String, Int)
toDB (User name password credit) =
    (name, password ^. Password.salt, password ^. Password.hash, credit)
