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

module Luna.Interpreter.Proto.CompileError where

import qualified Flowbox.Batch.Project.Project               as Project
import           Flowbox.Data.Convert                        hiding (Error)
import           Flowbox.Prelude                             hiding (error)
import qualified Generated.Proto.Interpreter.CompileError    as Gen
import           Luna.Interpreter.Proto.CallPointPath        ()
import           Luna.Interpreter.Session.Data.CallPointPath (CallPointPath)
import           Luna.Interpreter.Session.Error              (Error)
import qualified Luna.Interpreter.Session.Error              as Error



instance ConvertPure ((Project.ID, CallPointPath), Error) Gen.CompileError where
    encodeP (cpp, error) = Gen.CompileError (encodeP cpp) (encodeP $ Error.format error)
