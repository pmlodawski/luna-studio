---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
module Flowbox.System.IO.Serializer(
    Serializable(..),

    serializeMany,
    serialize
) where

import           System.Directory       as Dir
import qualified System.IO              as IO

import qualified Flowbox.System.UniPath as UniPath
import           Flowbox.System.UniPath   (UniPath)


data Serializable = Serializable UniPath (IO.Handle -> IO())


serializeMany :: [Serializable] -> IO()
serializeMany serializables = do
    sequence_ $ map (\s -> serialize s) serializables
    return ()


serialize :: Serializable -> IO()
serialize (Serializable upath save) = do
    let foldername = UniPath.toUnixString $ init upath
        filename   = UniPath.toUnixString upath
    Dir.createDirectoryIfMissing True foldername
    IO.withFile filename IO.WriteMode save

