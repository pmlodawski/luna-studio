---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.FileManager.Handler.FileSystem where

import qualified Flowbox.AWS.S3.Directory                             as Directory
import qualified Flowbox.AWS.S3.File                                  as File
import           Flowbox.Batch.Tools.Serialize.Proto.Conversion.Item  ()
import           Flowbox.Prelude
import           Flowbox.System.Log.Logger
import           Flowbox.Tools.Serialize.Proto.Conversion.Basic
import qualified Generated.Proto.FileManager.FileSystem.CP.Request    as CP
import qualified Generated.Proto.FileManager.FileSystem.CP.Update     as CP
import qualified Generated.Proto.FileManager.FileSystem.LS.Request    as LS
import qualified Generated.Proto.FileManager.FileSystem.LS.Status     as LS
import qualified Generated.Proto.FileManager.FileSystem.MkDir.Request as MkDir
import qualified Generated.Proto.FileManager.FileSystem.MkDir.Update  as MkDir
import qualified Generated.Proto.FileManager.FileSystem.MV.Request    as MV
import qualified Generated.Proto.FileManager.FileSystem.MV.Update     as MV
import qualified Generated.Proto.FileManager.FileSystem.RM.Request    as RM
import qualified Generated.Proto.FileManager.FileSystem.RM.Update     as RM
import qualified Generated.Proto.FileManager.FileSystem.Stat.Request  as Stat
import qualified Generated.Proto.FileManager.FileSystem.Stat.Status   as Stat
import qualified Generated.Proto.FileManager.FileSystem.Touch.Request as Touch
import qualified Generated.Proto.FileManager.FileSystem.Touch.Update  as Touch



loggerIO :: LoggerIO
loggerIO = getLoggerIO "Flowbox.FileManager.Handler.FileSystem"

------ public api -------------------------------------------------

ls :: LS.Request -> IO LS.Status
ls (LS.Request tpath) = do
    let path = decodeP tpath
    items <- BatchFS.ls path
    return $ LS.Status (encodeList items) tpath


stat :: Stat.Request -> IO Stat.Status
stat (Stat.Request tpath) = do
    let path = decodeP tpath
    item  <- BatchFS.stat path
    return $ Stat.Status (encode item) tpath


mkdir :: MkDir.Request -> IO MkDir.Update
mkdir (MkDir.Request tpath) = do
    let path = decodeP tpath
    Directory.create path
    return $ MkDir.Update tpath


touch :: Touch.Request -> IO Touch.Update
touch (Touch.Request tpath) = do
    let path = decodeP tpath
    File.touch path
    return $ Touch.Update tpath


rm :: RM.Request -> IO RM.Update
rm (RM.Request tpath) = do
    let path = decodeP tpath
    Directory.rm path
    return $ RM.Update tpath


cp :: CP.Request -> IO CP.Update
cp (CP.Request tsrc tdst) = do
    let usrc = decodeP tsrc
    let udst = decodeP tdst
    BatchFS.cp usrc udst
    return $ CP.Update tsrc tdst


mv :: MV.Request -> IO MV.Update
mv (MV.Request tsrc tdst) = do
    let usrc = decodeP tsrc
    let udst = decodeP tdst
    BatchFS.mv usrc udst
    return $ MV.Update tsrc tdst
