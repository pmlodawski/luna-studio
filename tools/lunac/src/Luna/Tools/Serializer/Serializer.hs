---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
module Luna.Tools.Serializer.Serializer(
    Serializable(..),

    serializeMany,
    serialize
) where

import System.Directory
import System.IO

import qualified Luna.System.UniPath         as UniPath
import           Luna.System.UniPath           (UniPath)


data Serializable = Serializable UniPath (Handle -> IO())


serializeMany :: [Serializable] -> IO()
serializeMany serializables = do
    sequence_ $ map (\s -> serialize s) serializables
    return ()


serialize :: Serializable -> IO()
serialize (Serializable upath save) = do
    let foldername = UniPath.toUnixString $ init upath
        filename   = UniPath.toUnixString upath
    createDirectoryIfMissing True foldername
    withFile filename WriteMode save

