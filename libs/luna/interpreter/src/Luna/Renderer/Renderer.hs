---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Flowbox Team <contact@flowbox.io>, 2014
-- Proprietary and confidential
-- Unauthorized copying of this file, via any medium is strictly prohibited
---------------------------------------------------------------------------
{-# LANGUAGE ScopedTypeVariables #-}

module Luna.Renderer.Renderer where

import           Control.Monad (forM, forM_)
import qualified Data.IntSet   as IntSet

import           Flowbox.Data.MapForest                      (MapForest)
import           Flowbox.Prelude
import qualified Luna.Interpreter.Session.AST.Executor       as Executor
import qualified Luna.Interpreter.Session.Cache.Invalidate   as Invalidate
import qualified Luna.Interpreter.Session.Cache.Value        as Value
import           Luna.Interpreter.Session.Data.CallPoint     (CallPoint)
import           Luna.Interpreter.Session.Data.CallPointPath (CallPointPath)
import           Luna.Interpreter.Session.Data.Time          (Time)
import qualified Luna.Interpreter.Session.Data.VarName       as VarName
import qualified Luna.Interpreter.Session.Env                as Env
import           Luna.Interpreter.Session.Error              (Error)
import           Luna.Interpreter.Session.Memory.Manager     (MemoryManager)
import           Luna.Interpreter.Session.Session            (Session)
import qualified Luna.Interpreter.Session.Session            as Session
import           Luna.Renderer.Data.FrameRange               (FrameRanges)
import qualified Luna.Renderer.Data.FrameRange               as FrameRange



type ProgressReporter = Int -> Int -> IO ()


render :: MemoryManager mm
       => FrameRanges -> ProgressReporter -> Session mm [(Int, MapForest CallPoint Error)]
render frameRanges progressReporter = do
    let frames     = IntSet.toList $ FrameRange.frames frameRanges
        iFrames    = zip [1..] frames
        progress i = liftIO $ progressReporter i $ length frames
    progress 0
    forM iFrames $ \(i, frame) -> do
        Env.setTimeVar $ fromIntegral frame
        Invalidate.modifyTimeRefs
        errors <- snd <$> Executor.processMain
        progress i
        return (frame, errors)


renderNode :: MemoryManager mm
           => CallPointPath -> FrameRanges
           -> ProgressReporter -> Session mm ()
renderNode callPointPath frameRanges progressReporter = do
    let frames     = IntSet.toList $ FrameRange.frames frameRanges
        iFrames    = zip [1..] frames
        progress i = liftIO $ progressReporter i $ length frames
    varName <- Value.getVarName callPointPath
    let expr = "\\_time -> do { _ <- toIOEnv (fromValue (" <> VarName.toString varName <> " _time)) ; return () }"
    (action :: Time -> IO ()) <- Session.interpret expr
    progress 0
    forM_ iFrames $ \(i, frame) -> liftIO $ do
        action $ fromIntegral frame
        progress i
