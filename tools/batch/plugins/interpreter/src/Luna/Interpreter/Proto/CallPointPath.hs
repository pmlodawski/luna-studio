---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Luna.Interpreter.Proto.CallPointPath where

import qualified Flowbox.Batch.Project.Project                  as Project
import           Flowbox.Control.Error
import           Flowbox.Prelude
import           Flowbox.Tools.Serialize.Proto.Conversion.Basic
import qualified Generated.Proto.Interpreter.CallPointPath      as Gen
import           Luna.Interpreter.Proto.CallPoint               ()
import           Luna.Interpreter.Session.Data.CallPointPath    (CallPointPath)



instance Convert (Project.ID, CallPointPath) Gen.CallPointPath where
    encode (projectID, callPointPath) =
        Gen.CallPointPath (encodePJ projectID) (encodeList callPointPath)
    decode (Gen.CallPointPath mtprojectID tpath) = do
        projectID <- decodeP <$> mtprojectID <?> "Failed to decode CallPointPath: 'projectID' field is missing"
        path      <- decodeList tpath
        return (projectID, path)
