---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types       #-}
{-# LANGUAGE TemplateHaskell  #-}

module Luna.Build.Source.File where

import           Control.Applicative
import           Control.Monad.RWS
import qualified Data.List           as List
import           Data.String.Utils   (replace)
import qualified Data.String.Utils   as StringUtils
import qualified Data.Text.Lazy      as Text
import qualified System.IO           as IO

import           Flowbox.Prelude                    hiding (error, id)
import qualified Flowbox.System.Directory.Directory as Directory
import           Flowbox.System.Log.Logger
import           Flowbox.System.UniPath             (UniPath)
import qualified Flowbox.System.UniPath             as UniPath
import           Luna.Data.Source                   (Code (Code), File (File), Source (Source))
import Luna.Syntax.Name.Path (QualPath(QualPath))



path2module :: UniPath -> QualPath
path2module upath = QualPath (init path) (last path) where
    path = map Text.pack $ UniPath.toList $ UniPath.dropExtension upath


getSource :: MonadIO m => UniPath -> UniPath -> m (Source File)
getSource rootPath' path' = do
    path     <- UniPath.expand path'
    rootPath <- UniPath.expand rootPath'
    let mpath = path2module $ UniPath.makeRelative rootPath path
    return $ Source mpath (File $ Text.pack $ UniPath.toUnixString path)


--readSource :: MonadIO m => UniPath -> UniPath -> m Source
--readSource rootPath path = do
--    filename <- UniPath.toUnixString <$> UniPath.expand path
--    txt      <- liftIO $ IO.readFile filename
--    let m    = getModule rootPath path
--        code = tabs2spaces txt
--    return $ Source m code


tabs2spaces :: String -> String
tabs2spaces = replace "\t" "    "


module2path :: QualPath -> String -> UniPath
module2path m ext = UniPath.addExtension ext $ UniPath.fromList $ map Text.unpack $ toList m


writeSource :: MonadIO m => UniPath -> String -> Source Code -> m ()
writeSource uRootPath ext (Source m (Code content)) = do
    rootPath <- UniPath.expand uRootPath
    let fileName   = UniPath.fromList $ (UniPath.toList rootPath) ++ (UniPath.toList $ module2path m ext)
        folderName = UniPath.basePath fileName
    liftIO $ do Directory.createDirectoryIfMissing True folderName
                IO.writeFile (UniPath.toUnixString fileName) $ Text.unpack content
