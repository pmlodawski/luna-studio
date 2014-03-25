---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.FileManager.Handler.FileSystem where

import qualified Flowbox.Batch.Handler.FileSystem                    as BatchFS
import           Flowbox.Batch.Tools.Serialize.Proto.Conversion.Item ()
import           Flowbox.Prelude
import           Flowbox.System.Log.Logger
import           Flowbox.Tools.Serialize.Proto.Conversion.Basic
import qualified Generated.Proto.FileManager.FileSystem.CP.Args      as CP
import qualified Generated.Proto.FileManager.FileSystem.CP.Result    as CP
import qualified Generated.Proto.FileManager.FileSystem.LS.Args      as LS
import qualified Generated.Proto.FileManager.FileSystem.LS.Result    as LS
import qualified Generated.Proto.FileManager.FileSystem.MkDir.Args   as MkDir
import qualified Generated.Proto.FileManager.FileSystem.MkDir.Result as MkDir
import qualified Generated.Proto.FileManager.FileSystem.MV.Args      as MV
import qualified Generated.Proto.FileManager.FileSystem.MV.Result    as MV
import qualified Generated.Proto.FileManager.FileSystem.RM.Args      as RM
import qualified Generated.Proto.FileManager.FileSystem.RM.Result    as RM
import qualified Generated.Proto.FileManager.FileSystem.Stat.Args    as Stat
import qualified Generated.Proto.FileManager.FileSystem.Stat.Result  as Stat
import qualified Generated.Proto.FileManager.FileSystem.Touch.Args   as Touch
import qualified Generated.Proto.FileManager.FileSystem.Touch.Result as Touch



loggerIO :: LoggerIO
loggerIO = getLoggerIO "Flowbox.FileManager.Handler.FileSystem"

------ public api -------------------------------------------------

ls :: LS.Args -> IO LS.Result
ls (LS.Args tpath) = do
    let path = decodeP tpath
    items <- BatchFS.ls path
    return $ LS.Result $ encodeList items


stat :: Stat.Args -> IO Stat.Result
stat (Stat.Args tpath) = do
    let path = decodeP tpath
    item  <- BatchFS.stat path
    return $ Stat.Result $ encode item


mkdir :: MkDir.Args -> IO MkDir.Result
mkdir (MkDir.Args tpath) = do
    let path = decodeP tpath
    BatchFS.mkdir path
    return MkDir.Result


touch :: Touch.Args -> IO Touch.Result
touch (Touch.Args tpath) = do
    let path = decodeP tpath
    BatchFS.touch path
    return Touch.Result


rm :: RM.Args -> IO RM.Result
rm (RM.Args tpath) = do
    let path = decodeP tpath
    BatchFS.rm path
    return RM.Result


cp :: CP.Args -> IO CP.Result
cp (CP.Args tsrc tdst) = do
    let usrc = decodeP tsrc
    let udst = decodeP tdst
    BatchFS.cp usrc udst
    return CP.Result


mv :: MV.Args -> IO MV.Result
mv (MV.Args tsrc tdst) = do
    let usrc = decodeP tsrc
    let udst = decodeP tdst
    BatchFS.mv usrc udst
    return MV.Result
