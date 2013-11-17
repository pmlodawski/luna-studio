---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
module Flowbox.Batch.Server.ZMQ.Handlers.FileSystem (
    ls,
    stat,

    mkdir,
    touch,
    rm,
    cp,
    mv,
) 
where


import           Data.IORef                                            (IORef)

import           Flowbox.Prelude                                       
import qualified Flowbox.Batch.Handlers.FileSystem                   as BatchFS
import           Flowbox.Batch.Batch                                   (Batch)
import           Flowbox.Batch.Tools.Serialize.Proto.Conversion.Item   ()
import           Flowbox.Control.Error                                 
import           Flowbox.System.Log.Logger                             
import           Flowbox.Tools.Conversion.Proto                        
import qualified Generated.Proto.FileSystem.LS.Args                  as LS
import qualified Generated.Proto.FileSystem.LS.Result                as LS
import qualified Generated.Proto.FileSystem.Stat.Args                as Stat
import qualified Generated.Proto.FileSystem.Stat.Result              as Stat
import qualified Generated.Proto.FileSystem.MkDir.Args               as MkDir
import qualified Generated.Proto.FileSystem.MkDir.Result             as MkDir
import qualified Generated.Proto.FileSystem.Touch.Args               as Touch
import qualified Generated.Proto.FileSystem.Touch.Result             as Touch
import qualified Generated.Proto.FileSystem.RM.Args                  as RM
import qualified Generated.Proto.FileSystem.RM.Result                as RM
import qualified Generated.Proto.FileSystem.CP.Args                  as CP
import qualified Generated.Proto.FileSystem.CP.Result                as CP
import qualified Generated.Proto.FileSystem.MV.Args                  as MV
import qualified Generated.Proto.FileSystem.MV.Result                as MV
import qualified Flowbox.Tools.Serialize.Proto.Conversion.List as Conv
import  Flowbox.Tools.Serialize.Proto.Conversion.UniPath ()



loggerIO :: LoggerIO
loggerIO = getLoggerIO "Flowbox.Batch.Server.ZMQ.Handlers.FileSystem"

------ public api -------------------------------------------------

ls :: IORef Batch -> LS.Args -> Script LS.Result
ls _ (LS.Args tpath) = do
    scriptIO $ loggerIO info "called ls"
    upath <- tryRight $ decode tpath
    scriptIO $ loggerIO debug $ "path: " ++ (show upath)
    items <- scriptIO $ BatchFS.ls upath
    return $ LS.Result $ Conv.encodeList items


stat :: IORef Batch -> Stat.Args -> Script Stat.Result
stat _ (Stat.Args tpath) = do
    scriptIO $ loggerIO info "called stat"
    upath <- tryRight $ decode tpath
    scriptIO $ loggerIO debug $ "path: " ++ (show upath)
    item  <- scriptIO $ BatchFS.stat upath
    return $ Stat.Result $ Just $ encode item


mkdir :: IORef Batch -> MkDir.Args -> Script MkDir.Result
mkdir _ (MkDir.Args tpath) = do
    scriptIO $ loggerIO info "called mkdir"
    upath <- tryRight $ decode tpath
    scriptIO $ loggerIO debug $ "path: " ++ (show upath)
    scriptIO $ BatchFS.mkdir upath
    return $ MkDir.Result


touch :: IORef Batch -> Touch.Args -> Script Touch.Result
touch _ (Touch.Args tpath) = do
    scriptIO $ loggerIO info "called touch"
    upath <- tryRight $ decode tpath
    scriptIO $ loggerIO debug $ "path: " ++ (show upath)
    scriptIO $ BatchFS.touch upath
    return $ Touch.Result


rm :: IORef Batch -> RM.Args -> Script RM.Result
rm _ (RM.Args tpath) = do
    scriptIO $ loggerIO info "called rm"
    upath <- tryRight $ decode tpath
    scriptIO $ loggerIO debug $ "path: " ++ (show upath)
    scriptIO $ BatchFS.rm upath
    return $ RM.Result


cp :: IORef Batch -> CP.Args -> Script CP.Result
cp _ (CP.Args tsrc tdst) = do
    scriptIO $ loggerIO info "called cp"
    usrc <- tryRight $ decode tsrc
    udst <- tryRight $ decode tdst
    scriptIO $ loggerIO debug $ "src: " ++ (show usrc) ++ " dst: " ++ (show udst)
    scriptIO $ BatchFS.cp usrc udst
    return $ CP.Result


mv :: IORef Batch -> MV.Args -> Script MV.Result
mv _ (MV.Args tsrc tdst) = do
    scriptIO $ loggerIO info "called mv"
    usrc <- tryRight $ decode tsrc
    udst <- tryRight $ decode tdst
    scriptIO $ loggerIO debug $ "src: " ++ (show usrc) ++ " dst: " ++ (show udst)
    scriptIO $ BatchFS.mv usrc udst
    return $ MV.Result
