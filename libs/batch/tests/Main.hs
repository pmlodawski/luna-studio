---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

import           Control.Monad.RWS          hiding (join)
import           Control.Monad.Trans.Either
import           System.TimeIt

import           Flowbox.Luna.Lib.Library                              (Library (Library))
import qualified Flowbox.Luna.Data.PropertyMap                             as PropertyMap
import qualified Flowbox.Luna.Passes.Build.Build                           as Build
import qualified Flowbox.Luna.Passes.General.Luna.Luna                     as Luna
import           Flowbox.Prelude
import           Flowbox.System.Log.Logger
import qualified Flowbox.System.Log.Logger                                 as Logger
import qualified Flowbox.System.UniPath                                    as UniPath
import qualified Flowbox.Luna.Tools.Serialize.Proto.Library as LibSerialization



logger :: LoggerIO
logger = getLoggerIO "Flowbox"


main :: IO ()
main = do
    Logger.setLevel DEBUG "Flowbox"
    out <- timeIt main_inner
    case out of
        Right _ -> return ()
        Left  e -> putStrLn e



main_inner :: IO (Either String ())
main_inner = Luna.run $ do
    let rootPath = "samples/VisualStd"
        filePath = "samples/VisualStd/Std.luna"
    (ast, _) <- hoistEither =<< Build.parseFile (UniPath.fromUnixString rootPath)
                                                (UniPath.fromUnixString filePath)
    let name = "Std"
        path = UniPath.fromUnixString "~/.flowbox/visual/stdlib.lunalib"
        library = Library name path ast PropertyMap.empty
    liftIO $ LibSerialization.storeLibrary library
    return ()
