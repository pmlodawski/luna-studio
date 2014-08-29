---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Flowbox.FileManager.RPC.Handler.File where

import qualified Flowbox.AWS.S3.File                                        as File
import           Flowbox.Bus.RPC.RPC                                        (RPC)
import           Flowbox.FileManager.Context                                (Context)
import qualified Flowbox.FileManager.Context                                as Context
import           Flowbox.Prelude                                            hiding (Context)
import           Flowbox.System.Log.Logger
import           Flowbox.Tools.Serialize.Proto.Conversion.Basic
import qualified Generated.Proto.FileManager.FileSystem.File.Copy.Request   as Copy
import qualified Generated.Proto.FileManager.FileSystem.File.Copy.Update    as Copy
import qualified Generated.Proto.FileManager.FileSystem.File.Exists.Request as Exists
import qualified Generated.Proto.FileManager.FileSystem.File.Exists.Status  as Exists
import qualified Generated.Proto.FileManager.FileSystem.File.Fetch.Request  as Fetch
import qualified Generated.Proto.FileManager.FileSystem.File.Fetch.Status   as Fetch
import qualified Generated.Proto.FileManager.FileSystem.File.Move.Request   as Move
import qualified Generated.Proto.FileManager.FileSystem.File.Move.Update    as Move
import qualified Generated.Proto.FileManager.FileSystem.File.Remove.Request as Remove
import qualified Generated.Proto.FileManager.FileSystem.File.Remove.Update  as Remove
import qualified Generated.Proto.FileManager.FileSystem.File.Upload.Request as Upload
import qualified Generated.Proto.FileManager.FileSystem.File.Upload.Status  as Upload



loggerIO :: LoggerIO
loggerIO = getLoggerIO "Flowbox.FileManager.RPC.Handler.File"

------ public api -------------------------------------------------


upload :: Upload.Request -> RPC Context IO Upload.Status
upload (Upload.Request tpath) = do
    let path = decodeP tpath
    Context.run $ File.upload "." path
    return $ Upload.Status tpath


fetch :: Fetch.Request -> RPC Context IO Fetch.Status
fetch (Fetch.Request tpath) = do
    let path = decodeP tpath
    Context.run $ File.fetch "." path
    return $ Fetch.Status tpath


exists :: Exists.Request -> RPC Context IO Exists.Status
exists (Exists.Request tpath) = do
    let path = decodeP tpath
    e <- Context.run $ File.exists path
    return $ Exists.Status e tpath


remove :: Remove.Request -> RPC Context IO Remove.Update
remove (Remove.Request tpath) = do
    let path = decodeP tpath
    Context.run $ File.remove path
    return $ Remove.Update tpath


copy :: Copy.Request -> RPC Context IO Copy.Update
copy (Copy.Request tsrc tdst) = do
    let src = decodeP tsrc
        dst = decodeP tdst
    Context.run $ File.copy src dst
    return $ Copy.Update tsrc tdst


move :: Move.Request -> RPC Context IO Move.Update
move (Move.Request tsrc tdst) = do
    let src = decodeP tsrc
        dst = decodeP tdst
    Context.run $ File.rename src dst
    return $ Move.Update tsrc tdst
