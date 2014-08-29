---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}

module Flowbox.Batch.Project.Project where

import           Flowbox.Prelude        hiding (empty)
import           Flowbox.System.UniPath (UniPath)
import qualified Flowbox.System.UniPath as UniPath
import           Luna.Graph.Attributes  (Attributes)
import qualified Luna.Graph.Attributes  as Attributes
import           Luna.Lib.Manager       (LibManager)
import qualified Luna.Lib.Manager       as LibManager



data Project = Project { _name     :: String
                       , _path     :: UniPath
                       , _libPaths :: [UniPath]
                       , _libs     :: LibManager
                       , _attrs    :: Attributes
                       } deriving (Show, Read)

makeLenses(''Project)

type ID = Int


empty :: Project
empty = Project "" UniPath.empty [] LibManager.empty Attributes.empty


make :: String -> UniPath -> Attributes -> Project
make name' path' attrs' = empty & name  .~ name'
                                & path  .~ path'
                                & attrs .~ attrs'
