---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Flowbox.FileManager.Handler.Directory where

import qualified System.Directory as Directory

import           Flowbox.Bus.RPC.RPC                                             (RPC)
import           Flowbox.Control.Error
import           Flowbox.Prelude                                                 hiding (Context)
import qualified Flowbox.System.Directory.Directory                              as FDirectory
import           Flowbox.System.Log.Logger
import qualified Flowbox.System.UniPath                                          as UniPath
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
logger = getLoggerIO "Flowbox.FileManager.Handler.Directory"

------ public api -------------------------------------------------


upload :: Upload.Request -> RPC IO Upload.Status
upload (Upload.Request tpath) =
    return $ Upload.Status tpath


fetch :: Fetch.Request -> RPC IO Fetch.Status
fetch (Fetch.Request tpath) =
    return $ Fetch.Status tpath


create :: Create.Request -> RPC IO Create.Update
create (Create.Request tpath) = do
    let path = decodeP tpath
    safeLiftIO $ Directory.createDirectory path
    return $ Create.Update tpath


exists :: Exists.Request -> RPC IO Exists.Status
exists (Exists.Request tpath) = do
    let path = decodeP tpath
    e <- safeLiftIO $ Directory.doesDirectoryExist path
    return $ Exists.Status e tpath


list :: List.Request -> RPC IO List.Status
list (List.Request tpath) = do
    let path = decodeP tpath
    contents <- safeLiftIO $ Directory.getDirectoryContents path
    return $ List.Status (encodeListP contents) tpath


remove :: Remove.Request -> RPC IO Remove.Update
remove (Remove.Request tpath) = do
    let path = decodeP tpath
    safeLiftIO $ Directory.removeDirectory path
    return $ Remove.Update tpath


copy :: Copy.Request -> RPC IO Copy.Update
copy (Copy.Request tsrc tdst) = do
    let src = decodeP tsrc
        dst = decodeP tdst
    safeLiftIO $ FDirectory.copyDirectoryRecursive (UniPath.fromUnixString src) (UniPath.fromUnixString dst)
    return $ Copy.Update tsrc tdst


move :: Move.Request -> RPC IO Move.Update
move (Move.Request tsrc tdst) = do
    let src = decodeP tsrc
        dst = decodeP tdst
    safeLiftIO $ Directory.renameDirectory src dst
    return $ Move.Update tsrc tdst
