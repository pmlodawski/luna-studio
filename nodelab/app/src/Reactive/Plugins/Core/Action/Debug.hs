{-# LANGUAGE OverloadedStrings #-}

module Reactive.Plugins.Core.Action.Debug where

import           Utils.PreludePlus
import           JS.Debug (clog, saveState, shouldExportState)

import qualified Event.Debug        as Debug
import           Event.Event        (Event(..))

import qualified Reactive.State.Global          as Global
import           Reactive.Commands.Command      (Command, performIO)
import           Control.Monad.State

import           Data.Aeson (encode, toJSON)
import           GHCJS.Types (JSVal)
import           GHCJS.Marshal (toJSVal)

toAction :: Event -> Maybe (Command Global.State ())
toAction (Debug Debug.GetState) = Just $ do
    state <- get
    let json = toJSON state
    performIO $ do
        val <- toJSVal json
        clog val
        saveState val
toAction _ = Just $ do
    when shouldExportState $ do
        state <- get
        let json = toJSON state
        performIO $ do
            val <- toJSVal json
            saveState val

