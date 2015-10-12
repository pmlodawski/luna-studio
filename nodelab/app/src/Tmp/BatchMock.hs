module Tmp.BatchMock where

import Utils.PreludePlus
import Batch.Workspace
import Batch.Project
import Batch.Breadcrumbs
import Batch.Expressions
import Batch.Library

fakeLibrary :: Library
fakeLibrary  = Library "fake" "super/fake" 42

fakeProject :: Project
fakeProject  = Project (Just "FAKE PROJECT")
                       "omg/it/is/so/fake/I/cant/even"
                       2137
                       [fakeLibrary]

fakeCrumbs :: Breadcrumbs
fakeCrumbs = Breadcrumbs [Module "FAKEMODULE"]

fakeWorkspace :: Workspace
fakeWorkspace = Workspace fakeProject fakeLibrary fakeCrumbs Fresh False
