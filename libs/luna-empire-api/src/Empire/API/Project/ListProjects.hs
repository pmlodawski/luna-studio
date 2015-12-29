module Empire.API.Project.ListProjects where

import Prologue
import Data.Binary                      (Binary)


data ListProjects = ListProjects deriving (Generic, Show, Eq)

instance Binary ListProjects


-- listProjects :: Empire [(ProjectId, Project)]
