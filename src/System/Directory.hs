-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE CPP #-}
{-# LANGUAGE PackageImports #-}

module System.Directory (
    module System.Directory,
    module X,
) where

import          "directory" System.Directory as X
import           System.FilePath        ((</>))
#ifdef mingw32_HOST_OS
import qualified System.Win32 as Win32
import qualified System.Environment as Environment
#endif

import           Flowbox.Prelude        




getAppDataDirectory :: IO FilePath
getAppDataDirectory = do
#if defined(mingw32_HOST_OS)
    Win32.sHGetFolderPath Win32.nullPtr Win32.cSIDL_APPDATA Win32.nullPtr 0
#else
    home <- X.getHomeDirectory
    return $ home </> ".local" </> "share"
#endif


getLocalAppDataDirectory :: IO FilePath
getLocalAppDataDirectory = do
#if defined(mingw32_HOST_OS)
    Environment.getEnv "LOCALAPPDATA"
#else
    home <- X.getHomeDirectory
    return $ home </> ".local" </> "share"
#endif


getAppDotFlowbox :: IO FilePath
getAppDotFlowbox = do
    home <- X.getHomeDirectory
    return $ home </> ".flowbox"


getAppFlowbox :: IO FilePath
getAppFlowbox = do
    home <- X.getHomeDirectory
    return $ home </> "flowbox"