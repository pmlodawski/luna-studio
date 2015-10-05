{-# LANGUAGE JavaScriptFFI #-}

module JS.Connection where

import           Utils.PreludePlus
import           Utils.Vector

import           Data.Dynamic
import           Data.Text.Lazy      (Text)

import           GHCJS.Foreign


foreign import javascript unsafe "app.displayCurrentConnection($1, $2, $3, $4)"
    displayCurrentConnection :: Double -> Double -> Double -> Double -> IO ()

foreign import javascript unsafe "app.removeCurrentConnection()"
    removeCurrentConnection :: IO ()

foreign import javascript unsafe "app.createConnection($1, $2, $3)"
    createConnection :: Int -> Int -> Int -> IO ()

foreign import javascript unsafe "app.updateConnection($1, $2, $3, $4, $5, $6)"
    updateConnection :: Int -> Bool -> Double -> Double -> Double -> Double -> IO ()

