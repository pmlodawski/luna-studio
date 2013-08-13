---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Flowbox.Luna.Tools.Serialize.Thrift.Conversion.Libs where

import           Data.Text.Lazy                                              (pack, unpack)

import qualified Libs_Types                                                as TLibs
import qualified Flowbox.System.UniPath                                    as UniPath
import qualified Flowbox.Luna.Network.Def.DefManager                       as DefManager
import           Flowbox.Luna.Network.Def.DefManager                         (DefManager(..))
import           Flowbox.Luna.Lib.Library                                  as Library
import           Flowbox.Luna.Tools.Serialize.Thrift.Conversion.Conversion   



instance Convert (Int, Library) (TLibs.Library, DefManager) where
    encode (libID, Library aname apath adefs) = (TLibs.Library tlibID tname tpath trootNodeDefID, adefs) where
        tlibID = Just $ itoi32 libID
        tname  = Just $ pack aname
        tpath  = Just $ pack $ UniPath.toUnixString apath
        trootNodeDefID = Just $ itoi32 Library.rootDefID
    decode (TLibs.Library mtlibID mtname mtpath _, adefs) = case mtlibID of
        Nothing            -> Left "`libID` field is missing."
        Just tlibID        -> case mtname of 
            Nothing        -> Left "`name` field is missing."
            Just tname     -> case mtpath of 
                Nothing    ->  Left "`path` field is missing."
                Just tpath -> Right (libID, Library aname apath adefs) where
                    aname = unpack tname
                    apath = UniPath.fromUnixString $ unpack tpath
                    libID = i32toi tlibID

