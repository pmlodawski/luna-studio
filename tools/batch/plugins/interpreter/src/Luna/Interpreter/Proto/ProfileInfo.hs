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

module Luna.Interpreter.Proto.ProfileInfo where

import qualified Flowbox.Batch.Project.Project               as Project
import           Flowbox.Data.Convert
import qualified Generated.Proto.Interpreter.ProfileInfo     as Gen
import           Luna.Interpreter.Proto.CallPointPath        ()
import           Luna.Interpreter.Session.Data.CallPointPath (CallPointPath)
import           Luna.Interpreter.Session.Profile.Info       (ProfileInfo (ProfileInfo))



instance ConvertPure ((Project.ID, CallPointPath), ProfileInfo) Gen.ProfileInfo where
    encodeP (cpp, ProfileInfo totalCpuTime totalRealTime compileTime computeTime) =
        Gen.ProfileInfo (encodeP cpp) totalCpuTime (encodeP totalRealTime) (encodeP compileTime) (encodeP computeTime)
    decodeP (Gen.ProfileInfo cpp totalCpuTime totalRealTime compileTime computeTime) =
        (decodeP cpp, ProfileInfo totalCpuTime (decodeP totalRealTime) (decodeP compileTime) (decodeP computeTime))
