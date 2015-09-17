module Batch.Workspace where

import Utils.PreludePlus
import Batch.Project
import Batch.Library
import Batch.Breadcrumbs

data InterpreterState = Fresh
                      | AllSet
                      deriving (Show, Eq)

data Workspace = Workspace { _project          :: Project
                           , _library          :: Library
                           , _breadcrumbs      :: Breadcrumbs
                           , _interpreterState :: InterpreterState
                           } deriving (Show, Eq)

makeLenses ''Workspace

instance PrettyPrinter Workspace where
    display workspace = "Workspace(" <> show (workspace ^. project)
                              <> "," <> show (workspace ^. library)
                              <> "," <> show (workspace ^. breadcrumbs)
                              <> ")"
