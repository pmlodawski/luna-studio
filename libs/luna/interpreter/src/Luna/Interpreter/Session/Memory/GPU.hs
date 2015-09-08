---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Luna.Interpreter.Session.Memory.GPU where


import           Flowbox.Prelude
import           Luna.Interpreter.Session.Session (Session)
import qualified Luna.Interpreter.Session.Session as Session




performGC :: Session mm ()
performGC = Session.withImports [ "Flowbox.Graphics.Memory" ]
    $ Session.runStmt "performGC"
