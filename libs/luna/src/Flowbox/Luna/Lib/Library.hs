---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Luna.Lib.Library(
    Library(..),
    ID,
    empty,
    rootDefID,
    make
) where

import           Flowbox.Prelude
import qualified Flowbox.System.UniPath              as UniPath
import           Flowbox.System.UniPath                (UniPath)
import qualified Flowbox.Luna.Network.Def.DefManager as DefManager
import           Flowbox.Luna.Network.Def.DefManager   (DefManager)
import qualified Flowbox.Luna.Network.Def.Definition as Definition




data Library =  Library{ name :: String
                       , path :: UniPath
                       , defs :: DefManager
                       } deriving (Show)

type ID  = Int


empty :: Library
empty = Library "" UniPath.empty DefManager.empty


rootDefID :: ID
rootDefID = 0


make :: String -> UniPath -> Library
make name' path' = empty { name = name'
                         , path = path'
                         , defs = DefManager.insNode (rootDefID, rootdef) DefManager.empty
                         } where
    rootdef = Definition.mkModule name'


