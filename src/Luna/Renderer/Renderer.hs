---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Flowbox Team <contact@flowbox.io>, 2014
-- Proprietary and confidential
-- Unauthorized copying of this file, via any medium is strictly prohibited
---------------------------------------------------------------------------
module Luna.Renderer.Renderer where

import           Control.Monad (forM)
import qualified Data.IntSet   as IntSet

import           Flowbox.Data.MapForest                    (MapForest)
import           Flowbox.Prelude
import qualified Luna.Interpreter.Session.AST.Executor     as Executor
import qualified Luna.Interpreter.Session.Cache.Invalidate as Invalidate
import           Luna.Interpreter.Session.Data.CallPoint   (CallPoint)
import qualified Luna.Interpreter.Session.Env              as Env
import           Luna.Interpreter.Session.Error            (Error)
import           Luna.Interpreter.Session.Memory.Manager   (MemoryManager)
import           Luna.Interpreter.Session.Session          (Session)
import           Luna.Renderer.Data.FrameRange             (FrameRanges)
import qualified Luna.Renderer.Data.FrameRange             as FrameRange



render :: MemoryManager mm
       => FrameRanges -> Session mm [(Int, MapForest CallPoint Error)]
render frameRanges = do
    let frames = IntSet.toList $ FrameRange.frames frameRanges
    forM frames $ \frame -> do
        Env.setTimeVar $ fromIntegral frame
        Invalidate.modifyTimeRefs
        errors <- snd <$> Executor.processMain
        return (frame, errors)
