module Reactive.Plugins.Core.Action.State.Backend where

import Utils.PreludePlus

data State = AwaitingConnection | AwaitingProject | AwaitingLibs | Ready | Fail deriving (Eq, Show)

instance Default State where
    def = AwaitingConnection

instance PrettyPrinter State where
    display st = "backendSt(" <> show st <> ")"
