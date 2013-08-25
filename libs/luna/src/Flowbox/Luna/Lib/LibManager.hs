---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Luna.Lib.LibManager(
    module Flowbox.Luna.Data.Graph,
    LibManager,
    empty,

    loadLibrary,
) where

import qualified Flowbox.Luna.Tools.Serialize.Lib as LibSerialization
import qualified Flowbox.Luna.Data.Graph          as DG
import           Flowbox.Luna.Data.Graph          hiding (Graph, Edge, empty)
import qualified Flowbox.Luna.Lib.Library         as Library
import           Flowbox.Luna.Lib.Library           (Library)
import           Flowbox.Luna.Lib.Edge              (Edge)
import           Flowbox.System.UniPath             (UniPath)


type LibManager = DG.Graph Library Edge


empty :: LibManager
empty = DG.empty


loadLibrary :: UniPath -> LibManager -> IO (LibManager, (Library.ID, Library))
loadLibrary apath libManager = do
    library <- LibSerialization.restoreLibrary apath
    let (newLibManager, libID) = insNewNode library libManager
    return (newLibManager, (libID, library))

