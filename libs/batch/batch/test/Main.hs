---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}

import Control.Monad.RWS          hiding (join)
import Control.Monad.Trans.Either
import System.TimeIt

import           Flowbox.Data.Version                  ()
import           Flowbox.Prelude
import           Flowbox.System.Log.Logger
import qualified Flowbox.System.Log.Logger             as Logger
import qualified Flowbox.System.UniPath                as UniPath
import qualified Luna.DEP.Data.Serialize.Proto.Library as LibSerialization
import qualified Luna.DEP.Graph.PropertyMap            as PropertyMap
import           Luna.DEP.Lib.Lib                      (Library (Library))
import qualified Luna.DEP.Pass.Build.Build             as Build
import qualified Luna.DEP.Pass.General.Luna.Luna       as Luna



logger :: LoggerIO
logger = getLoggerIO $(moduleName)


main :: IO ()
main = do
    Logger.setLevel DEBUG "Flowbox"
    out <- timeIt main_inner
    case out of
        Right _ -> return ()
        Left  e -> putStrLn e



main_inner :: IO (Either String ())
main_inner = Luna.run $ do
    let rootPath = "stdlib/visual"
        filePath = "stdlib/visual/Std.luna"
    (ast, _, _) <- hoistEither =<< Build.parseFile (UniPath.fromUnixString rootPath)
                                                   (UniPath.fromUnixString filePath)
    let name = "Std"
        path = UniPath.fromUnixString "~/.flowbox/visual/stdlib.lunalib"
        library = Library name def path ast PropertyMap.empty
    liftIO $ LibSerialization.storeLibrary library Nothing
    return ()
