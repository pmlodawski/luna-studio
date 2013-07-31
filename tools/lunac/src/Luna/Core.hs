---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Luna.Core(
Core(..),
empty
) where

import qualified Luna.Lib.LibManager         as LibManager
import           Luna.Lib.LibManager           (LibManager)
import qualified Luna.Network.Def.DefManager as DefManager
import           Luna.Network.Def.DefManager   (DefManager)



data Core = Core {
    libManager :: LibManager,
    defManager :: DefManager
} deriving(Show)

empty :: Core
empty = Core LibManager.empty DefManager.empty