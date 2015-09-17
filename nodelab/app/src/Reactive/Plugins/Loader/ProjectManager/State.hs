module Reactive.Plugins.Loader.ProjectManager.State where

import Utils.PreludePlus
import Batch.Project

data State = AwaitingProject String
           | AwaitingLibs Project
           | Ready Project
           | AfterInitialize
