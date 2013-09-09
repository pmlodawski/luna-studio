---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Flowbox.Luna.Tools.Serialize.Thrift.Conversion.Libs where

import           Flowbox.Prelude                       
import           Data.Text.Lazy                        (pack, unpack)

import qualified Libs_Types                          as TLibs
import           Flowbox.Control.Error                 
import qualified Flowbox.Luna.Lib.Library            as Library
import           Flowbox.Luna.Lib.Library              (Library(Library))
import           Flowbox.Luna.Network.Def.DefManager   (DefManager)
import qualified Flowbox.System.UniPath              as UniPath
import           Flowbox.Tools.Conversion              


instance Convert (Int, Library) (TLibs.Library, DefManager) where
    encode (libID, Library name path defs) = (TLibs.Library mtlibID mtname mtpath mtrootNodeDefID, defs) where
        mtlibID = Just $ itoi32 libID
        mtname  = Just $ pack name
        mtpath  = Just $ pack $ UniPath.toUnixString path
        mtrootNodeDefID = Just $ itoi32 Library.rootDefID
    decode (TLibs.Library mtlibID mtname mtpath _, defs) = do 
        tlibID <- mtlibID <?> "Failed to decode Library: `libID` field is missing."
        tname  <- mtname  <?> "Failed to decode Library: `name` field is missing."
        tpath  <- mtpath  <?> "Failed to decode Library: `path` field is missing."
        let name = unpack tname
            path = UniPath.fromUnixString $ unpack tpath
            libID = i32toi tlibID

        return (libID, Library name path defs)

