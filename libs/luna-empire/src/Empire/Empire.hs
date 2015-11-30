module Empire.Empire where

import           Prologue
import           Empire.Data.Project (Project, ProjectId)
import           Control.Monad.State
import           Control.Monad.Error (ErrorT(..), runErrorT)
import           Data.IntMap         (IntMap)
import qualified Data.IntMap         as IntMap

type Error = String

type ProjectManager = IntMap Project

newtype EmpireEnv = EmpireEnv { _projectManager :: ProjectManager } deriving Show

instance Default EmpireEnv where
    def = EmpireEnv IntMap.empty

makeLenses ''EmpireEnv

type Empire' s a = ErrorT Error (StateT s IO) a

type Empire a = Empire' EmpireEnv a

runEmpire :: s -> Empire' s a -> IO (Either Error a, s)
runEmpire st cmd = runStateT (runErrorT cmd) st

execEmpire :: s -> Empire' s a -> IO (Either Error a)
execEmpire st cmd = fst <$> runEmpire st cmd

empire :: (s -> IO (Either Error a, s)) -> Empire' s a
empire = ErrorT . StateT
