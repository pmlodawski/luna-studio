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

import qualified Data.Graph.Inductive        as DG
import           Luna.Lib.Edge                 (Edge)
import qualified Luna.Lib.Library            as Library
import           Luna.Lib.Library              (Library)
import           Luna.Common.Graph             (newNodes)
import qualified Luna.Common.Graph           as CommonG
import           Luna.Network.Def.DefManager   (DefManager)
import qualified Luna.Network.Def.DefManager as DefManager

data LibManager = LibManager {
	repr       :: DG.Gr Library Edge,
	defmanager :: DefManager
} deriving (Show)

empty :: LibManager
empty = LibManager DG.empty DefManager.empty

newIds :: LibManager -> [Library.ID]
newIds manager = newNodes $ repr manager

register :: DG.LNode Library -> LibManager -> LibManager
register libnode manager = manager {repr=DG.insNode libnode $ repr manager}

reload :: Library.ID -> LibManager -> LibManager
reload = undefined

libnodeById :: LibManager -> Library.ID -> DG.LNode Library 
libnodeById manager lid = CommonG.lnodeById (repr manager) lid

libById :: LibManager -> Library.ID -> Library
libById manager lid = CommonG.nodeById (repr manager) lid