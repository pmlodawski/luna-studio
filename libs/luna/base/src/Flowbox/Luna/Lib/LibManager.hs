---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Flowbox.Luna.Lib.LibManager (
    module Flowbox.Data.Graph,
    LibManager,
    empty,

    loadLibrary,
) where

import           Flowbox.Data.Graph                         hiding (Edge, empty)
import qualified Flowbox.Data.Graph                         as Graph
import           Flowbox.Luna.Lib.Edge                      (Edge)
import           Flowbox.Luna.Lib.Library                   (Library)
import qualified Flowbox.Luna.Lib.Library                   as Library
import qualified Flowbox.Luna.Tools.Serialize.Proto.Library as LibSerialization
import           Flowbox.Prelude                            hiding (empty)
import           Flowbox.System.UniPath                     (UniPath)



type LibManager = Graph Library Edge


empty :: LibManager
empty = Graph.empty


loadLibrary :: UniPath -> LibManager -> IO (LibManager, (Library.ID, Library))
loadLibrary apath libManager = do
    library <- LibSerialization.restoreLibrary apath
    let (newLibManager, libID) = insNewNode library libManager
    return (newLibManager, (libID, library))
