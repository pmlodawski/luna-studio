---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Luna.Core(

) where


import Luna.Lib.LibManager (LibManager)
import Luna.Network.Def.DefManager (DefManager)


data Core = Core {
    libManager :: LibManager,
    defManager :: DefManager
} deriving(Show)

