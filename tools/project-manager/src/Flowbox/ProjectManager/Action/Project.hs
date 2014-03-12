---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.ProjectManager.Action.Project where

import Flowbox.Bus.Topic.Action                       (Action)
import Flowbox.Prelude                                hiding (Action)

import Flowbox.ProjectManager.Action.ProjectOperation (ProjectOperation)



data Project = Project ProjectOperation Action
             deriving (Read, Show)
