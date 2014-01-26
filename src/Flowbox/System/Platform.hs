-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Flowbox.System.Platform where

import qualified System.Info as Info

import Flowbox.Prelude hiding (error)



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
onPlatform platform f = if os == platform
                            then f
                            else return ()


onLinux :: Monad m => m () -> m ()
onLinux = onPlatform Linux


onWindows :: Monad m => m () -> m ()
onWindows = onPlatform Windows


dependent :: a -> a -> a -> a
dependent lin win other = case os of
    Linux   -> lin
    Windows -> win
    Other   -> other
