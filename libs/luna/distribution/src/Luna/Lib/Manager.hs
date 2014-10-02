---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Luna.Lib.Manager (
    module Flowbox.Data.Graph,
    LibManager,
    empty
) where

import           Flowbox.Data.Graph                hiding (Edge, empty)
import qualified Flowbox.Data.Graph                as Graph
import           Flowbox.Prelude                   hiding (empty)
import           Flowbox.System.UniPath            (UniPath)
--import qualified Luna.Data.Serialize.Proto.Library as LibSerialization
import           Luna.Lib.Edge                     (Edge)
import           Luna.Lib.Lib                      (Library)
import qualified Luna.Lib.Lib                      as Library



type LibManager = Graph Library Edge


empty :: LibManager
empty = Graph.empty

-- TODO[pm]: przeniesc do biblioteki serializacji!
--loadLibrary :: UniPath -> LibManager -> IO (LibManager, (Library.ID, Library))
--loadLibrary apath libManager = do
--    library <- LibSerialization.restoreLibrary apath
--    let (newLibManager, libID) = insNewNode library libManager
--    return (newLibManager, (libID, library))
