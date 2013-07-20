---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Luna.Network.Def.DefManager(
DefManager(..)
--empty
) where

import qualified Data.Graph.Inductive     as DG
import           Luna.Network.Def.NodeDef   (NodeDef)
import           Luna.Network.Def.Edge      (Edge)


data DefManager = DefManager{
    repr      :: DG.Gr NodeDef Edge
} deriving (Show)

-- empty :: DefManager
-- empty = DefManager Map.empty Graph.empty

