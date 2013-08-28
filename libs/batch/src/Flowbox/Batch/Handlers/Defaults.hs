---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Batch.Handlers.Defaults (
    nodeDefaults,
    setNodeDefault,
    removeNodeDefault,
) where

import           Data.Map                                  (Map)

import           Flowbox.Batch.Batch                       (Batch(..))
import qualified Flowbox.Batch.Project.Project           as Project
import qualified Flowbox.Luna.Lib.Library                as Library
import qualified Flowbox.Luna.Network.Def.Definition     as Definition
import           Flowbox.Luna.Network.Graph.DefaultValue   (DefaultValue)
import qualified Flowbox.Luna.Network.Graph.Node         as Node


defaultsKey :: String
defaultsKey = "Defaults-map"


nodeDefaults :: Node.ID -> Definition.ID -> Library.ID -> Project.ID -> Batch -> Either String (Map [Int] DefaultValue)
nodeDefaults nodeID defID libID projectID batch = undefined


setNodeDefault :: [Int] -> DefaultValue
               -> Node.ID -> Definition.ID -> Library.ID -> Project.ID -> Batch -> Either String Batch
setNodeDefault dstPort value nodeID defID libID projectID batch = undefined


removeNodeDefault :: [Int]
                  -> Node.ID -> Definition.ID -> Library.ID -> Project.ID -> Batch -> Either String Batch
removeNodeDefault dstPort nodeID defID libID projectID batch= undefined