---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Flowbox Team <contact@flowbox.io>, 2014
-- Proprietary and confidential
-- Unauthorized copying of this file, via any medium is strictly prohibited
---------------------------------------------------------------------------
module Luna.Interpreter.Session.GHC.Util where

import qualified GHC
import           Outputable (Outputable)
import qualified Outputable

import Flowbox.Prelude



dshow :: Outputable a => GHC.DynFlags -> a -> String
dshow dflags = Outputable.showSDoc dflags . Outputable.ppr
