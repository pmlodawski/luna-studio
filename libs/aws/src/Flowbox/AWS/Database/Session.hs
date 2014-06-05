---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.AWS.Database.Session where

import           Control.Monad
import qualified Data.Time                  as Time
import qualified Database.PostgreSQL.Simple as PSQL

import qualified Flowbox.AWS.Database.SQL.Session.Add            as SessionAdd
import qualified Flowbox.AWS.Database.SQL.Session.All            as SessionAll
import qualified Flowbox.AWS.Database.SQL.Session.Delete         as SessionDelete
import qualified Flowbox.AWS.Database.SQL.Session.DeleteByID     as SessionDeleteByID
import qualified Flowbox.AWS.Database.SQL.Session.DeleteByUser   as SessionDeleteByUser
import qualified Flowbox.AWS.Database.SQL.Session.FindByInstance as SessionFindByInstance
import qualified Flowbox.AWS.Database.SQL.Session.FindByUser     as SessionFindByUser
import qualified Flowbox.AWS.Database.SQL.Session.Update         as SessionUpdate
import qualified Flowbox.AWS.EC2.Instance.Instance               as Instance
import           Flowbox.AWS.User.Session                        (Session (Session))
import qualified Flowbox.AWS.User.Session                        as Session
import qualified Flowbox.AWS.User.User                           as User
import           Flowbox.Prelude                                 hiding (id)



create :: PSQL.Connection -> User.Name -> Instance.ID -> Time.UTCTime -> Session.Policy
       -> IO Session
create conn userName instanceID expires policy = do
    PSQL.Only id <- head <$> PSQL.query conn SessionAdd.query (userName, instanceID, expires, show policy)
    return $ Session id userName instanceID expires policy


findByInstance :: PSQL.Connection -> Instance.ID -> IO [Session]
findByInstance conn instanceID = PSQL.query conn
    SessionFindByInstance.query (PSQL.Only instanceID)


findByUser :: PSQL.Connection -> User.Name -> IO [Session]
findByUser conn userName = PSQL.query conn
    SessionFindByUser.query (PSQL.Only userName)


deleteByID :: PSQL.Connection -> Session.ID -> IO ()
deleteByID conn sessionID =
    void $ PSQL.execute conn SessionDeleteByID.query (PSQL.Only sessionID)


deleteByUser :: PSQL.Connection -> User.Name -> IO ()
deleteByUser conn userName =
    void $ PSQL.execute conn SessionDeleteByUser.query (PSQL.Only userName)


delete :: PSQL.Connection -> User.Name -> Instance.ID -> IO ()
delete conn userName instanceID =
    void $ PSQL.execute conn SessionDelete.query (userName, instanceID)


update :: PSQL.Connection -> Session -> IO ()
update conn session =
    void $ PSQL.execute conn SessionUpdate.query
         (session ^. Session.expires, show $ session ^. Session.policy, session ^. Session.id)


all :: PSQL.Connection -> IO [Session]
all conn = PSQL.query conn SessionAll.query ()

