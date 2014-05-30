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
import           Flowbox.AWS.User.User              (User)
import qualified Flowbox.AWS.User.User              as User
import           Flowbox.Prelude



add :: PSQL.Connection -> User -> IO ()
add connection user = PSQL.withTransaction connection $ do
    exising <- get connection $ user ^. User.name
    if Maybe.isJust exising
        then fail "User already exists"
        else void $ PSQL.execute connection (fromString UserAdd.query)
                        $ User.toDB user


get :: PSQL.Connection -> User.Name -> IO (Maybe User)
get connection userName = fmap User.fromDB . Maybe.listToMaybe <$> PSQL.query connection
    (fromString UserFind.query) (PSQL.Only userName)

