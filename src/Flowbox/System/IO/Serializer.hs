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

import           Control.Applicative
import           System.Directory    as Dir
import qualified System.IO           as IO

import           Flowbox.Prelude
import           Flowbox.System.UniPath (UniPath)
import qualified Flowbox.System.UniPath as UniPath



data Serializable = Serializable UniPath (IO.Handle -> IO())

data Deserializable a = Deserializable UniPath (IO.Handle -> IO a)


serializeMany :: [Serializable] -> IO()
serializeMany serializables = do
    sequence_ $ map (\s -> serialize s) serializables
    return ()


serialize :: Serializable -> IO()
serialize (Serializable upath save) = do
    apath <- UniPath.expand upath
    let foldername = UniPath.toUnixString $ init apath
        filename   = UniPath.toUnixString apath
    Dir.createDirectoryIfMissing True foldername
    IO.withFile filename IO.WriteMode save


deserialize :: Deserializable a -> IO a
deserialize (Deserializable upath load) = do
    filename <- UniPath.toUnixString <$> UniPath.expand upath
    IO.withFile filename IO.ReadMode load
