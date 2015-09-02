module Reactive.Plugins.Loader.ProjectManager.State where

import Utils.PreludePlus
import Batch.Project

data ConnectionState = AwaitingConnection
                     | AwaitingProject
                     | AwaitingLibs
                     | Ready
                     | AfterInitialize
                     | Fail
                     deriving (Eq, Show)

instance Default ConnectionState where
    def = AwaitingConnection

data State = State { _connection :: ConnectionState
                   , _project    :: Maybe Project
                   } deriving (Eq, Show)

instance Default State where
    def = State def def

makeLenses ''State

