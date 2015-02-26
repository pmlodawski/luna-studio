---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}

module Flowbox.System.IO.Serializer where

import           Control.Applicative
import qualified Flowbox.System.Directory.Directory as Dir
import qualified System.IO                          as IO

import           Flowbox.Prelude
import           Flowbox.System.UniPath (UniPath)
import qualified Flowbox.System.UniPath as UniPath



data Serializable = Serializable { _dstPath :: UniPath
                                 , _save    :: IO.Handle -> IO ()
                                 }

data Deserializable a = Deserializable { _srcPath :: UniPath
                                       , _load    :: IO.Handle -> IO a
                                       }

makeLenses ''Serializable
makeLenses ''Deserializable


serialize :: Serializable -> IO ()
serialize serializable = do
    let swpPath = UniPath.addExtension ".swp" $ serializable ^. dstPath
    serializeNonAtomic $ dstPath .~ swpPath $ serializable
    Dir.renameFile swpPath $ serializable ^. dstPath


serializeNonAtomic :: Serializable -> IO ()
serializeNonAtomic (Serializable upath save') = do
    apath <- UniPath.expand upath
    let foldername = UniPath.basePath apath
        filename   = UniPath.toUnixString apath
    Dir.createDirectoryIfMissing True foldername
    IO.withFile filename IO.WriteMode save'


deserialize :: Deserializable a -> IO a
deserialize (Deserializable upath load') = do
    filename <- UniPath.toUnixString <$> UniPath.expand upath
    IO.withFile filename IO.ReadMode load'
