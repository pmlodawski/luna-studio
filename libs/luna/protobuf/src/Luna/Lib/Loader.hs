---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Luna.Lib.Loader where

import           Flowbox.Prelude
import           Flowbox.System.UniPath            (UniPath)
import qualified Luna.Data.Serialize.Proto.Library as LibSerialization
import           Luna.Lib.Lib                      (Library)
import qualified Luna.Lib.Lib                      as Library
import           Luna.Lib.Manager                  (LibManager, insNewNode)



loadLibrary :: UniPath -> LibManager -> IO (LibManager, (Library.ID, Library))
loadLibrary apath libManager = do
    library <- LibSerialization.restoreLibrary apath
    let (newLibManager, libID) = insNewNode library libManager
    return (newLibManager, (libID, library))
