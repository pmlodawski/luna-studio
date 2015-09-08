---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Luna.DEP.Lib.Path where

import           Flowbox.Prelude
import qualified Flowbox.System.Directory as Directory
import           Flowbox.System.UniPath   (UniPath)
import qualified Flowbox.System.UniPath   as UniPath



data Location = Local | Global deriving (Read, Show, Eq)


libData :: String
libData = "libdata"


sandboxRoot :: UniPath
sandboxRoot = UniPath.fromUnixString ".luna-sandbox"


userRoot :: UniPath
userRoot = UniPath.fromUnixString "$APPDATA/flowbox"


globalRoot :: UniPath
globalRoot = UniPath.fromUnixString "$LUNAROOT"


localRoot :: IO UniPath
localRoot = do
    inSandbox <- sandboxExists
    return $ if inSandbox then sandboxRoot else userRoot


mkLibData :: UniPath -> UniPath
mkLibData = UniPath.append libData


sandboxExists :: IO Bool
sandboxExists = Directory.doesDirectoryExist sandboxRoot


sandboxCreate :: IO ()
sandboxCreate = Directory.createDirectoryIfMissing True sandboxRoot


searchPaths :: IO [UniPath]
searchPaths = do
    local <- localRoot
    return $ map mkLibData $ local : [globalRoot]


installPath :: Location -> IO UniPath
installPath Local  = mkLibData <$> localRoot
installPath Global = mkLibData <$> return globalRoot
