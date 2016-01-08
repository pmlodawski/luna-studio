module FakeMock where

import Utils.PreludePlus
import Batch.Workspace
import Batch.Breadcrumbs
import Batch.Expressions
import qualified Data.IntMap.Lazy as IntMap
import Empire.API.Data.Project
import Empire.API.Data.Library

fakeLibrary :: Library
fakeLibrary  = Library (Just "fake") "super/fake"

fakeProject :: Project
fakeProject  = Project (Just "FAKE PROJECT")
                       "omg/it/is/so/fake/I/cant/even"
                       (IntMap.fromList [(0, fakeLibrary)])

-- fakeCrumbs :: Breadcrumbs
-- fakeCrumbs = Breadcrumbs [Module "FAKEMODULE"]

fakeWorkspace :: Workspace
fakeWorkspace = Workspace (IntMap.fromList [(0, fakeProject)]) 0 0  Fresh False
