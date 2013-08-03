---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Luna.Tools.Serialization.Libs where

import           Data.Text.Lazy   (pack, unpack)

import qualified Libs_Types               as TLibs
import           Luna.Lib.Library         as Library
import           Luna.Tools.Serialization
import qualified Luna.System.UniPath      as UniPath

instance Serialize (Int, Library) TLibs.Library where
    encode (libID, Library aname apath arootNodeDefID) = TLibs.Library tlibID tname tpath trootNodeDefID where
        tlibID = Just $ itoi32 libID
        tname  = Just $ pack aname
        tpath  = Just $ pack $ UniPath.toUnixString apath
        trootNodeDefID = Just $ itoi32 arootNodeDefID
    decode (TLibs.Library (Just tlibID) (Just tname) (Just tpath) (Just trootNodeDefID)) = 
        Right (libID, Library aname apath arootNodeDefID) where
            aname  = unpack tname
            apath  = UniPath.fromUnixString $ unpack tpath
            libID = i32toi tlibID
            arootNodeDefID = i32toi trootNodeDefID
    decode (TLibs.Library (Just _) (Just _) (Just _) Nothing ) = Left "`rootNodeDefID` field is missing."
    decode (TLibs.Library (Just _) (Just _) Nothing  _       ) = Left "`path` field is missing."
    decode (TLibs.Library (Just _) Nothing  _        _       ) = Left "`name` field is missing."
    decode (TLibs.Library Nothing  _        _        _       ) = Left "`libID` field is missing."

