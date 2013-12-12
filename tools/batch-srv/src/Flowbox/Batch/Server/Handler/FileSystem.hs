---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
module Flowbox.Batch.Server.Handler.FileSystem (
    ls,
    stat,

    mkdir,
    touch,
    rm,
    cp,
    mv,
)
where

import Data.IORef (IORef)

import           Flowbox.Batch.Batch                                 (Batch)
import qualified Flowbox.Batch.Handler.FileSystem                    as BatchFS
import           Flowbox.Batch.Tools.Serialize.Proto.Conversion.Item ()
import           Flowbox.Control.Error
import           Flowbox.Prelude
import           Flowbox.System.Log.Logger
import           Flowbox.Tools.Serialize.Proto.Conversion.Basic
import qualified Generated.Proto.Batch.FileSystem.CP.Args            as CP
import qualified Generated.Proto.Batch.FileSystem.CP.Result          as CP
import qualified Generated.Proto.Batch.FileSystem.LS.Args            as LS
import qualified Generated.Proto.Batch.FileSystem.LS.Result          as LS
import qualified Generated.Proto.Batch.FileSystem.MkDir.Args         as MkDir
import qualified Generated.Proto.Batch.FileSystem.MkDir.Result       as MkDir
import qualified Generated.Proto.Batch.FileSystem.MV.Args            as MV
import qualified Generated.Proto.Batch.FileSystem.MV.Result          as MV
import qualified Generated.Proto.Batch.FileSystem.RM.Args            as RM
import qualified Generated.Proto.Batch.FileSystem.RM.Result          as RM
import qualified Generated.Proto.Batch.FileSystem.Stat.Args          as Stat
import qualified Generated.Proto.Batch.FileSystem.Stat.Result        as Stat
import qualified Generated.Proto.Batch.FileSystem.Touch.Args         as Touch
import qualified Generated.Proto.Batch.FileSystem.Touch.Result       as Touch



loggerIO :: LoggerIO
loggerIO = getLoggerIO "Flowbox.Batch.Server.Handler.FileSystem"

------ public api -------------------------------------------------

ls :: IORef Batch -> LS.Args -> Script LS.Result
ls _ (LS.Args tpath) = do
    scriptIO $ loggerIO info "called ls"
    let upath = decodeP tpath
    items <- scriptIO $ BatchFS.ls upath
    return $ LS.Result $ encodeList items


stat :: IORef Batch -> Stat.Args -> Script Stat.Result
stat _ (Stat.Args tpath) = do
    scriptIO $ loggerIO info "called stat"
    let upath = decodeP tpath
    scriptIO $ loggerIO debug $ "path: " ++ (show upath)
    item  <- scriptIO $ BatchFS.stat upath
    return $ Stat.Result $ encode item


mkdir :: IORef Batch -> MkDir.Args -> Script MkDir.Result
mkdir _ (MkDir.Args tpath) = do
    scriptIO $ loggerIO info "called mkdir"
    let upath = decodeP tpath
    scriptIO $ loggerIO debug $ "path: " ++ (show upath)
    scriptIO $ BatchFS.mkdir upath
    return MkDir.Result


touch :: IORef Batch -> Touch.Args -> Script Touch.Result
touch _ (Touch.Args tpath) = do
    scriptIO $ loggerIO info "called touch"
    let upath = decodeP tpath
    scriptIO $ loggerIO debug $ "path: " ++ (show upath)
    scriptIO $ BatchFS.touch upath
    return Touch.Result


rm :: IORef Batch -> RM.Args -> Script RM.Result
rm _ (RM.Args tpath) = do
    scriptIO $ loggerIO info "called rm"
    let upath = decodeP tpath
    scriptIO $ loggerIO debug $ "path: " ++ (show upath)
    scriptIO $ BatchFS.rm upath
    return RM.Result


cp :: IORef Batch -> CP.Args -> Script CP.Result
cp _ (CP.Args tsrc tdst) = do
    scriptIO $ loggerIO info "called cp"
    let usrc = decodeP tsrc
    let udst = decodeP tdst
    scriptIO $ loggerIO debug $ "src: " ++ (show usrc) ++ " dst: " ++ (show udst)
    scriptIO $ BatchFS.cp usrc udst
    return CP.Result


mv :: IORef Batch -> MV.Args -> Script MV.Result
mv _ (MV.Args tsrc tdst) = do
    scriptIO $ loggerIO info "called mv"
    let usrc = decodeP tsrc
    let udst = decodeP tdst
    scriptIO $ loggerIO debug $ "src: " ++ (show usrc) ++ " dst: " ++ (show udst)
    scriptIO $ BatchFS.mv usrc udst
    return MV.Result
