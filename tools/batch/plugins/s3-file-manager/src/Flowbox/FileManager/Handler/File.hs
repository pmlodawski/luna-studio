---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Flowbox.FileManager.Handler.File where

import qualified Flowbox.AWS.S3.File                                        as File
import qualified Flowbox.AWS.S3.S3                                          as S3
import           Flowbox.FileManager.Context                                (Context)
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
loggerIO = getLoggerIO "Flowbox.FileManager.Handler.File"

------ public api -------------------------------------------------


upload :: Context -> Upload.Request -> IO Upload.Status
upload ctx (Upload.Request tpath) = do
    let path = decodeP tpath
    S3.runS3env ctx $ File.upload "." path
    return $ Upload.Status tpath


fetch :: Context -> Fetch.Request -> IO Fetch.Status
fetch ctx (Fetch.Request tpath) = do
    let path = decodeP tpath
    S3.runS3env ctx $ File.fetch "." path
    return $ Fetch.Status tpath


exists :: Context -> Exists.Request -> IO Exists.Status
exists ctx (Exists.Request tpath) = do
    let path = decodeP tpath
    e <- S3.runS3env ctx $ File.exists path
    return $ Exists.Status e tpath


remove :: Context -> Remove.Request -> IO Remove.Update
remove ctx (Remove.Request tpath) = do
    let path = decodeP tpath
    S3.runS3env ctx $ File.remove path
    return $ Remove.Update tpath


copy :: Context -> Copy.Request -> IO Copy.Update
copy ctx (Copy.Request tsrc tdst) = do
    let src = decodeP tsrc
        dst = decodeP tdst
    S3.runS3env ctx $ File.copy src dst
    return $ Copy.Update tsrc tdst


move :: Context -> Move.Request -> IO Move.Update
move ctx (Move.Request tsrc tdst) = do
    let src = decodeP tsrc
        dst = decodeP tdst
    S3.runS3env ctx $ File.rename src dst
    return $ Move.Update tsrc tdst
