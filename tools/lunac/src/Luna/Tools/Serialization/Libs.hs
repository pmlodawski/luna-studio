---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Luna.Tools.Serialization.Libs where


import qualified Data.HashMap.Strict as Map
import qualified Data.Text.Lazy      as Text

import qualified Libs_Types
import           Luna.Lib.Library         as Library
import qualified Luna.Lib.Library           (Library)
import           Luna.Tools.Serialization
import qualified Luna.System.UniPath      as UniPath

instance Serialize (Int, Library) Libs_Types.Library where
    encode (libID, Library name path rootNodeDefID) = Libs_Types.Library tlibID tname tpath trootNodeDefID where
        tlibID = Just $ itoi32 libID
        tname  = Just $ Text.pack name
        tpath  = Just $ Text.pack $ UniPath.toUnixString path
        trootNodeDefID = Just $ itoi32 rootNodeDefID
    decode (Libs_Types.Library (Just tlibID) (Just tname) (Just tpath) (Just trootNodeDefID)) = 
        Right (libID, Library name path rootNodeDefID) where
            name  = Text.unpack tname
            path  = UniPath.fromUnixString $ Text.unpack tpath
            libID = i32toi tlibID
            rootNodeDefID = i32toi trootNodeDefID
    decode (Libs_Types.Library {}) = 
        Left "Some fields are missing."

