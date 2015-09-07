module Reactive.Plugins.Loader.ProjectManager.State where

import Utils.PreludePlus
import Batch.Project

data State = AwaitingConnection
           | AwaitingProject
           | AwaitingLibs Project
           | Ready Project
           | AfterInitialize

instance Default State where
    def = AwaitingConnection

