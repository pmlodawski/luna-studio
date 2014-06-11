-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Flowbox.System.Platform where

import qualified System.Info as Info

import Control.Monad (when)

import           Flowbox.Prelude        hiding (error)
import           Flowbox.System.UniPath (UniPath)
import qualified Flowbox.System.UniPath as UniPath



data Platform = Linux
              | Windows
              | Other
              deriving (Eq, Show, Read)


os :: Platform
os = case Info.os of
    "linux"   -> Linux
    "mingw32" -> Windows
    _         -> Other


onPlatform :: Monad m => Platform -> m () -> m ()
onPlatform platform = when (os == platform)


onLinux :: Monad m => m () -> m ()
onLinux = onPlatform Linux


onWindows :: Monad m => m () -> m ()
onWindows = onPlatform Windows


dependent :: a -> a -> a -> a
dependent lin win other = case os of
    Linux   -> lin
    Windows -> win
    Other   -> other


addExeOnWindows :: UniPath -> UniPath
addExeOnWindows path = dependent path windowsPath path where
    exe = ".exe"
    windowsPath = if UniPath.extension path == exe
        then path
        else UniPath.setExtension exe path
