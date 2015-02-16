---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}

module Flowbox.Batch.Project.Project where

import Flowbox.Prelude
import Flowbox.System.UniPath    (UniPath)
import Luna.DEP.Graph.Attributes (Attributes)
import Luna.DEP.Lib.Manager      (LibManager)



data Project = Project { _name     :: Maybe String
                       , _path     :: UniPath
                       , _libPaths :: [UniPath]
                       , _libs     :: LibManager
                       , _attrs    :: Attributes
                       } deriving (Show, Read)

makeLenses ''Project


newtype ID = ID { toInt :: Int }
           deriving (Show, Ord, Eq)


instance Default Project where
    def = Project Nothing def def def def


make :: Maybe String -> UniPath -> Attributes -> Project
make name' path' attrs' = def & name  .~ name'
                              & path  .~ path'
                              & attrs .~ attrs'
