---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.AWS.Database.User where

import           Control.Monad
import qualified Data.Maybe                 as Maybe
import           Database.PostgreSQL.Simple ((:.) ((:.)))
import qualified Database.PostgreSQL.Simple as PSQL

import qualified Flowbox.AWS.Database.SQL.User.Add    as UserAdd
import qualified Flowbox.AWS.Database.SQL.User.All    as UserAll
import qualified Flowbox.AWS.Database.SQL.User.Find   as UserFind
import qualified Flowbox.AWS.Database.SQL.User.Update as UserUpdate
import           Flowbox.AWS.User.User                (User)
import qualified Flowbox.AWS.User.User                as User
import           Flowbox.Prelude



add :: PSQL.Connection -> User -> IO ()
add conn user =
    void $ PSQL.execute conn UserAdd.query user


find :: PSQL.Connection -> User.Name -> IO (Maybe User)
find conn userName = Maybe.listToMaybe
    <$> PSQL.query conn UserFind.query (PSQL.Only userName)


update :: PSQL.Connection -> User -> IO ()
update conn user =
    void $ PSQL.execute conn UserUpdate.query
         $ user :. (PSQL.Only $ user ^. User.name)

all :: PSQL.Connection -> IO [User]
all conn = PSQL.query conn UserAll.query ()
