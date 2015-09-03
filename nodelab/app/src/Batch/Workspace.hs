module Batch.Workspace where

import Utils.PreludePlus
import Batch.Project
import Batch.Library
import Batch.Breadcrumbs

data Workspace = Workspace { _project     :: Project
                           , _library     :: Library
                           , _breadcrumbs :: Breadcrumbs
                           } deriving (Show, Eq)

makeLenses ''Workspace

instance PrettyPrinter Workspace where
    display workspace = "Workspace(" <> show (workspace ^. project)
                              <> "," <> show (workspace ^. library)
                              <> "," <> show (workspace ^. breadcrumbs)
                              <> ")"
