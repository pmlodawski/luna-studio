---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Luna.Lib.Manager (
    module Flowbox.Data.Graph,
    module Luna.Lib.Manager,
) where

import           Flowbox.Data.Graph                hiding (Edge, delNode, empty, insNewNode, lab, labNodes, nodes, updateNode)
import qualified Flowbox.Data.Graph                as Graph
import           Flowbox.Prelude
import           Luna.Lib.Edge                     (Edge)
import           Luna.Lib.Lib                      (Library)
import qualified Luna.Lib.Lib                      as Library



type LibManager = Graph Library Edge


lab :: LibManager -> Library.ID -> Maybe Library
lab lm = Graph.lab lm . Library.toInt


labNodes :: LibManager -> [(Library.ID, Library)]
labNodes = over (mapped . _1) Library.ID . Graph.labNodes


nodes :: LibManager -> [Library.ID]
nodes = map Library.ID . Graph.nodes


updateNode :: (Library.ID, Library) -> LibManager -> LibManager
updateNode lib = Graph.updateNode (_1 %~ Library.toInt $ lib)


insNewNode :: Library -> LibManager -> (LibManager, Library.ID)
insNewNode lib lm = _2 %~ Library.ID $ Graph.insNewNode lib lm


delNode :: Library.ID -> LibManager -> LibManager
delNode libraryID = Graph.delNode $ Library.toInt libraryID
