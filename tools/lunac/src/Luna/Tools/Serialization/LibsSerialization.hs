---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Luna.Tools.Serialization.LibsSerialization where


import qualified Data.HashMap.Strict as Map
import Data.Int
import qualified Data.Text.Lazy      as Text

import qualified Libs_Types
import           Luna.Lib.Library         as Library
import qualified Luna.Lib.Library           (Library)
import           Luna.Tools.Serialization
import qualified Luna.System.UniPath      as UniPath

instance Serialize (Library, Int32) Libs_Types.Library where
    encode (Library name path, libID) = 
        Libs_Types.Library (Just libID) (Just $ Text.pack name) (Just $ Text.pack $ UniPath.toUnixString path)
    decode (Libs_Types.Library (Just libID) (Just  name) (Just path)) = 
        Right (Library (Text.unpack name) $ UniPath.fromUnixString $ Text.unpack path, libID)
    decode (Libs_Types.Library {}) = 
        Left "Some fields are missing."

