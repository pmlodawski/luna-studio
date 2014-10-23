---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Flowbox.FileManager.RPC.Handler.Directory where

import           Flowbox.Bus.RPC.RPC                                             (RPC)
import           Flowbox.FileManager.FileManager                                 (FileManager)
import qualified Flowbox.FileManager.FileManager                                 as FileManager
import           Flowbox.Prelude                                                 hiding (Context)
import           Flowbox.System.Log.Logger
import           Flowbox.Tools.Serialize.Proto.Conversion.Basic
import qualified Generated.Proto.FileManager.FileSystem.Directory.Copy.Request   as Copy
import qualified Generated.Proto.FileManager.FileSystem.Directory.Copy.Update    as Copy
import qualified Generated.Proto.FileManager.FileSystem.Directory.Create.Request as Create
import qualified Generated.Proto.FileManager.FileSystem.Directory.Create.Update  as Create
import qualified Generated.Proto.FileManager.FileSystem.Directory.Exists.Request as Exists
import qualified Generated.Proto.FileManager.FileSystem.Directory.Exists.Status  as Exists
import qualified Generated.Proto.FileManager.FileSystem.Directory.Fetch.Request  as Fetch
import qualified Generated.Proto.FileManager.FileSystem.Directory.Fetch.Status   as Fetch
import qualified Generated.Proto.FileManager.FileSystem.Directory.List.Request   as List
import qualified Generated.Proto.FileManager.FileSystem.Directory.List.Status    as List
import qualified Generated.Proto.FileManager.FileSystem.Directory.Move.Request   as Move
import qualified Generated.Proto.FileManager.FileSystem.Directory.Move.Update    as Move
import qualified Generated.Proto.FileManager.FileSystem.Directory.Remove.Request as Remove
import qualified Generated.Proto.FileManager.FileSystem.Directory.Remove.Update  as Remove
import qualified Generated.Proto.FileManager.FileSystem.Directory.Upload.Request as Upload
import qualified Generated.Proto.FileManager.FileSystem.Directory.Upload.Status  as Upload


logger :: LoggerIO
logger = getLoggerIO $(moduleName)

------ public api -------------------------------------------------


upload :: FileManager fm ctx => fm
       -> Upload.Request -> RPC ctx IO Upload.Status
upload fm request@(Upload.Request tpath) = do
    let path = decodeP tpath
    FileManager.uploadDirectory fm path
    return $ Upload.Status request


fetch :: FileManager fm ctx => fm
      -> Fetch.Request -> RPC ctx IO Fetch.Status
fetch fm request@(Fetch.Request tpath) = do
    let path = decodeP tpath
    FileManager.fetchDirectory fm path
    return $ Fetch.Status request


create :: FileManager fm ctx => fm
       -> Create.Request -> RPC ctx IO Create.Update
create fm request@(Create.Request tpath) = do
    let path = decodeP tpath
    FileManager.createDirectory fm path
    return $ Create.Update request


exists :: FileManager fm ctx => fm
       -> Exists.Request -> RPC ctx IO Exists.Status
exists fm request@(Exists.Request tpath) = do
    let path = decodeP tpath
    e <- FileManager.directoryExists fm path
    return $ Exists.Status request e


list :: FileManager fm ctx => fm
     -> List.Request -> RPC ctx IO List.Status
list fm request@(List.Request tpath) = do
    let path = decodeP tpath
    contents <- FileManager.listDirectory fm path
    return $ List.Status request (encodeListP contents)


remove :: FileManager fm ctx => fm
       -> Remove.Request -> RPC ctx IO Remove.Update
remove fm request@(Remove.Request tpath) = do
    let path = decodeP tpath
    FileManager.removeDirectory fm path
    return $ Remove.Update request


copy :: FileManager fm ctx => fm
     -> Copy.Request -> RPC ctx IO Copy.Update
copy fm request@(Copy.Request tsrc tdst) = do
    let src = decodeP tsrc
        dst = decodeP tdst
    FileManager.copyDirectory fm src dst
    return $ Copy.Update request


move :: FileManager fm ctx => fm
     -> Move.Request -> RPC ctx IO Move.Update
move fm request@(Move.Request tsrc tdst) = do
    let src = decodeP tsrc
        dst = decodeP tdst
    FileManager.moveDirectory fm src dst
    return $ Move.Update request
