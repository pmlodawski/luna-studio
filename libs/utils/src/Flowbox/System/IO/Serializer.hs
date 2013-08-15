---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
module Flowbox.System.IO.Serializer(
    Serializable(..),
    Deserializable(..),
    
    serializeMany,
    serialize,
    deserialize
) where

import           System.Directory       as Dir
import qualified System.IO              as IO

import qualified Flowbox.System.UniPath as UniPath
import           Flowbox.System.UniPath   (UniPath)


data Serializable = Serializable UniPath (IO.Handle -> IO())

data Deserializable a = Deserializable UniPath (IO.Handle -> IO (Either String a))


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


deserialize :: Deserializable a -> IO (Either String a)
deserialize (Deserializable upath load) = do
    let filename = UniPath.toUnixString upath
    IO.withFile filename IO.ReadMode load