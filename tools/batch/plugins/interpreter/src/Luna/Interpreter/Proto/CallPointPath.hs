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

import qualified Flowbox.Batch.Project.Project               as Project
import           Flowbox.Data.Convert
import qualified Generated.Proto.Interpreter.CallPointPath   as Gen
import           Luna.Interpreter.Proto.CallPoint            ()
import           Luna.Interpreter.Session.Data.CallPointPath (CallPointPath)



instance ConvertPure (Project.ID, CallPointPath) Gen.CallPointPath where
    encodeP (projectID, callPointPath) = Gen.CallPointPath (encodeP projectID) (encodeP callPointPath)
    decodeP (Gen.CallPointPath projectID path) = (decodeP projectID, decodeP path)
