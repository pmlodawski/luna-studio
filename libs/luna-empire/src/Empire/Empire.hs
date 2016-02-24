module Empire.Empire where

import           Prologue
import           Empire.Data.Project          (Project)
import           Empire.API.Data.Project      (ProjectId)
import           Empire.API.Data.AsyncUpdate  (AsyncUpdate)

import           Control.Monad.State
import           Control.Monad.Reader
import           Control.Monad.Error          (ErrorT(..), runErrorT, throwError, MonadError)
import           Data.IntMap                  (IntMap)
import qualified Data.IntMap                  as IntMap
import           Control.Concurrent.STM.TChan (TChan)

type Error = String

type ProjectManager = IntMap Project

newtype Env = Env { _projectManager :: ProjectManager } deriving Show
makeLenses ''Env

instance Default Env where
    def = Env IntMap.empty

newtype NotifierEnv = NotifierEnv { _updatesChan :: TChan AsyncUpdate }
makeLenses ''NotifierEnv

instance Show NotifierEnv where
    show _ = "NotifierEnv"

type Command s a = ErrorT Error (ReaderT NotifierEnv (StateT s IO)) a

type Empire a = Command Env a

runEmpire :: NotifierEnv -> s -> Command s a -> IO (Either Error a, s)
runEmpire notif st cmd = runStateT (runReaderT (runErrorT cmd) notif) st

execEmpire :: NotifierEnv -> s -> Command s a -> IO (Either Error a)
execEmpire = fmap fst .:. runEmpire

empire :: (NotifierEnv -> s -> IO (Either Error a, s)) -> Command s a
empire f = ErrorT readered where
    readered = ReaderT stated
    stated   = fmap StateT f

infixr 4 <?!>
(<?!>) :: MonadError Error m => m (Maybe a) -> Error -> m a
(<?!>) cmd err = cmd >>= maybe (throwError err) return
