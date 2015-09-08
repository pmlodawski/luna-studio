---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Luna.Renderer.Proto.FrameCompileError where

import qualified Flowbox.Batch.Project.Project              as Project
import           Flowbox.Data.Convert                       hiding (Error)
import           Flowbox.Data.MapForest                     (MapForest)
import qualified Flowbox.Data.MapForest                     as MapForest
import           Flowbox.Prelude
import qualified Generated.Proto.Renderer.FrameCompileError as Gen
import           Luna.Interpreter.Proto.CompileError        ()
import           Luna.Interpreter.Session.Data.CallPoint    (CallPoint)
import           Luna.Interpreter.Session.Error             (Error)



instance ConvertPure (Project.ID, (Int, MapForest CallPoint Error)) Gen.FrameCompileError where
    encodeP (projectID, (frame, compileErrors)) = Gen.FrameCompileError (encodeP frame) errors where
        errors = encodeP $ map (_1 %~ (projectID, )) $ MapForest.toList compileErrors
