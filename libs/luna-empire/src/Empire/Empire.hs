module Empire.Empire where

import Prologue
import Empire.Data.Project (Project, ProjectId)
import Control.Monad.State
import Control.Monad.Error (ErrorT, runErrorT)
import Data.IntMap         (IntMap)

type Error = String

type ProjectManager = IntMap Project

newtype EmpireEnv = EmpireEnv { _projectManager :: ProjectManager }

makeLenses ''EmpireEnv

type EmpireAction s a = ErrorT Error (StateT s IO) a

type Empire a = EmpireAction EmpireEnv a

runEmpire :: EmpireEnv -> Empire a -> IO (Either Error a)
runEmpire env cmd = fst <$> runStateT (runErrorT cmd) env
