{-# OPTIONS_GHC -fno-warn-unused-binds #-}

{-# LANGUAGE OverloadedStrings #-}

module Reactive.Plugins.Core.Action.Debug
    ( toAction
    , toActionEv
    ) where


import           Utils.PreludePlus
import           JS.Debug (clog, cinfo, saveState, lastEv, shouldExportState, processedEvent)

import qualified Event.Debug        as Debug
import           Event.Event        (Event(..))

import qualified Reactive.State.Global          as Global
import           Reactive.Commands.Command      (Command, performIO)
import           Control.Monad.State

import           Data.Aeson (toJSON)
import           GHCJS.Marshal (toJSVal)

toAction :: Event -> Maybe (Command Global.State ())
toAction (Debug Debug.GetState) = Just $ do
    state <- get
    let json = toJSON state
    performIO $ do
        val <- toJSVal json
        clog val
        saveState val
toAction ev = Just $ do
    -- logBatch ev
    when shouldExportState $ do
        state <- get
        let json = toJSON state
        performIO $ do
            val <- toJSVal json
            saveState val

toActionEv :: Event -> Maybe (Command Global.State ())
toActionEv ev = Just $ do
    -- Global.lastEvent ?= ev
    -- Global.eventNum  += 1
    when shouldExportState $ do
        evN <- use $ Global.eventNum
        performIO $ do
            processedEvent evN
            val <- toJSVal $ toJSON ev
            lastEv val

logBatch :: Event -> Command Global.State ()
logBatch (Batch e) = performIO $ do
    val <- toJSVal $ toJSON e
    cinfo val
logBatch _ = return ()
