---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Flowbox.Luna.Tools.Serialize.Thrift.Conversion.Libs where

import           Data.Text.Lazy                        (pack, unpack)

import qualified Libs_Types                          as TLibs
import           Flowbox.Control.Error                 
import qualified Flowbox.Luna.Network.Def.DefManager as DefManager
import           Flowbox.Luna.Network.Def.DefManager   (DefManager(..))
import           Flowbox.Luna.Lib.Library            as Library
import qualified Flowbox.System.UniPath              as UniPath
import           Flowbox.Tools.Conversion              



instance Convert (Int, Library) (TLibs.Library, DefManager) where
    encode (libID, Library aname apath adefs) = (TLibs.Library mtlibID mtname mtpath mtrootNodeDefID, adefs) where
        mtlibID = Just $ itoi32 libID
        mtname  = Just $ pack aname
        mtpath  = Just $ pack $ UniPath.toUnixString apath
        mtrootNodeDefID = Just $ itoi32 Library.rootDefID
    decode (TLibs.Library mtlibID mtname mtpath _, adefs) = do 
        tlibID <- mtlibID <?> "Failed to decode Library: `libID` field is missing."
        tname  <- mtname  <?> "Failed to decode Library: `name` field is missing."
        tpath  <- mtpath  <?> "Failed to decode Library: `path` field is missing."
        let aname = unpack tname
            apath = UniPath.fromUnixString $ unpack tpath
            libID = i32toi tlibID

        return (libID, Library aname apath adefs)

