---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.AWS.Database.Database where

import           Control.Monad
import           Data.String                (fromString)
import qualified Database.PostgreSQL.Simple as PSQL

import qualified Flowbox.AWS.Database.SQL.Schema as Schema
import           Flowbox.Prelude



type Database = PSQL.Connection

type Error = String


mk :: PSQL.ConnectInfo -> IO Database
mk = PSQL.connect


create :: PSQL.Connection -> IO ()
create connection = void $ PSQL.execute connection (fromString Schema.query) ()
