module Empire.Empire where

import           Prologue
import           Empire.Data.Project     (Project)
import           Empire.API.Data.Project (ProjectId)

import           Control.Monad.State
import           Control.Monad.Error     (ErrorT(..), runErrorT, throwError)
import           Data.IntMap             (IntMap)
import qualified Data.IntMap             as IntMap

type Error = String

type ProjectManager = IntMap Project

newtype EmpireEnv = EmpireEnv { _projectManager :: ProjectManager } deriving Show

instance Default EmpireEnv where
    def = EmpireEnv IntMap.empty

makeLenses ''EmpireEnv

type Command s a = ErrorT Error (StateT s IO) a

type Empire a = Command EmpireEnv a

runEmpire :: s -> Command s a -> IO (Either Error a, s)
runEmpire st cmd = runStateT (runErrorT cmd) st

execEmpire :: s -> Command s a -> IO (Either Error a)
execEmpire st cmd = fst <$> runEmpire st cmd

empire :: (s -> IO (Either Error a, s)) -> Command s a
empire = ErrorT . StateT

infixr 4 <?!>
(<?!>) :: Command s (Maybe a) -> Error -> Command s a
(<?!>) cmd err = cmd >>= maybe (throwError err) return
