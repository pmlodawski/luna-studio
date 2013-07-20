---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Luna.Lib.LibManager(
	LibManager(..),
	empty,
	newIds,
	register,
	reload
) where

import qualified Data.Graph.Inductive as DG
import           Luna.Lib.Edge          (Edge)
import qualified Luna.Lib.Library     as Library
import           Luna.Lib.Library       (Library, LibID, LibNode)
import           Luna.Common.Graph      (newNodes)
import qualified Luna.Common.Graph    as CommonG

data LibManager = LibManager {
	repr      :: DG.Gr Library Edge
} deriving (Show)

empty :: LibManager
empty = LibManager DG.empty

newIds :: LibManager -> [LibID]
newIds manager = newNodes $ repr manager

register :: LibNode -> LibManager -> LibManager
register libnode manager = manager {repr=DG.insNode libnode $ repr manager}

reload :: LibID -> LibManager -> LibManager
reload = undefined

libnodeById :: LibManager -> LibID -> LibNode
libnodeById manager lid = CommonG.lnodeById (repr manager) lid

libById :: LibManager -> LibID -> Library
libById manager lid = CommonG.nodeById (repr manager) lid