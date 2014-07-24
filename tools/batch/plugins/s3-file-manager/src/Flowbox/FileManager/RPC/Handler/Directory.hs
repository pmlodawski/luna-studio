---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Flowbox.FileManager.RPC.Handler.Directory where

import qualified Flowbox.AWS.S3.Directory                                        as Directory
import           Flowbox.Bus.RPC.RPC                                             (RPC)
import           Flowbox.FileManager.Context                                     (Context)
import qualified Flowbox.FileManager.Context                                     as Context
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



loggerIO :: LoggerIO
loggerIO = getLoggerIO "Flowbox.FileManager.RPC.Handler.Directory"

------ public api -------------------------------------------------


upload :: Upload.Request -> RPC Context IO Upload.Status
upload (Upload.Request tpath) = do
    let path = decodeP tpath
    Context.run $ Directory.upload "." path
    return $ Upload.Status tpath


fetch :: Fetch.Request -> RPC Context IO Fetch.Status
fetch (Fetch.Request tpath) = do
    let path = decodeP tpath
    Context.run $ Directory.fetch "." path
    return $ Fetch.Status tpath


create :: Create.Request -> RPC Context IO Create.Update
create (Create.Request tpath) = do
    let path = decodeP tpath
    Context.run $ Directory.create path
    return $ Create.Update tpath


exists :: Exists.Request -> RPC Context IO Exists.Status
exists (Exists.Request tpath) = do
    let path = decodeP tpath
    e <- Context.run $ Directory.exists path
    return $ Exists.Status e tpath


list :: List.Request -> RPC Context IO List.Status
list (List.Request tpath) = do
    let path = decodeP tpath
    contents <- Context.run $ Directory.getContents path
    return $ List.Status (encodeListP contents) tpath


remove :: Remove.Request -> RPC Context IO Remove.Update
remove (Remove.Request tpath) = do
    let path = decodeP tpath
    Context.run $ Directory.remove path
    return $ Remove.Update tpath


copy :: Copy.Request -> RPC Context IO Copy.Update
copy (Copy.Request tsrc tdst) = do
    let src = decodeP tsrc
        dst = decodeP tdst
    Context.run $ Directory.copy src dst
    return $ Copy.Update tsrc tdst


move :: Move.Request -> RPC Context IO Move.Update
move (Move.Request tsrc tdst) = do
    let src = decodeP tsrc
        dst = decodeP tdst
    Context.run $ Directory.copy src dst
    return $ Move.Update tsrc tdst
