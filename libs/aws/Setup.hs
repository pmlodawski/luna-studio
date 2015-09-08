---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

import           Control.Monad
import qualified Data.List                          as List
import           Distribution.PackageDescription
import           Distribution.Simple
import           Distribution.Simple.LocalBuildInfo
import           Distribution.Simple.PreProcess
import qualified Distribution.Verbosity             as Verbosity
import qualified System.FilePath                    as FilePath



main :: IO ()
main = defaultMainWithHooks
           simpleUserHooks{ hookedPreProcessors = [("sql", sql)] }


sql :: BuildInfo -> LocalBuildInfo -> PreProcessor
sql _ _ = PreProcessor
    { platformIndependent = True
    , runPreProcessor = mkSimplePreProcessor $ \inFile outFile verbosity -> do
        source <- readFile inFile
        let moduleName = List.intercalate "." $ tail $ FilePath.splitDirectories $ FilePath.dropExtension inFile
            contents = unlines
                [ "{-# LANGUAGE OverloadedStrings #-}"
                , "module " ++ moduleName ++ " where"
                , "import Database.PostgreSQL.Simple"
                , "import Data.String (fromString)"
                , "import Prelude"
                , "query :: Query"
                , "query = fromString $ unlines "
                , "    [\"" ++ List.intercalate "\"\n    , \"" (lines source) ++ "\"]"
                ]
        unless (verbosity == Verbosity.silent) $ do
            putStrLn $ "Preprocessing sql file: " ++ inFile  ++ "..."
            when (verbosity /= Verbosity.normal) $
                putStrLn contents
        writeFile outFile contents
    }
