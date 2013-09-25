---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
module Flowbox.Batch.Server.Handlers.FileSystem (
    ls,
    stat,

    mkdir,
    touch,
    rm,
    cp,
    mv,
) 
where


import           Data.IORef                                             
import           Data.Text.Lazy                                         (Text)
import qualified Data.Vector                                          as Vector
import           Data.Vector                                            (Vector)

import           Flowbox.Prelude                                        
import           Flowbox.Batch.Server.Handlers.Common                   (tRunScript)
import qualified Fs_Types                                             as TFS
import qualified Flowbox.Batch.Handlers.FileSystem                    as BatchFS
import           Flowbox.Batch.Batch                                    (Batch(..))
import           Flowbox.Batch.Tools.Serialize.Thrift.Conversion.Item   ()
import           Flowbox.Control.Error                                  
import           Flowbox.System.Log.Logger                              
import           Flowbox.Tools.Conversion                               



loggerIO :: LoggerIO
loggerIO = getLoggerIO "Flowbox.Batch.Server.Handlers.FileSystem"

------ public api -------------------------------------------------

ls :: IORef Batch -> Maybe Text -> IO (Vector TFS.FSItem)
ls _ mtpath = tRunScript $ do
    scriptIO $ loggerIO info "called ls"
    upath <- tryGetUniPath mtpath "path"
    scriptIO $ loggerIO debug $ "path: " ++ (show upath)
    items <- scriptIO $ BatchFS.ls upath
    let titems = map encode items
    return $ Vector.fromList titems


stat :: IORef Batch -> Maybe Text -> IO TFS.FSItem
stat _ mtpath = tRunScript $ do
    scriptIO $ loggerIO info "called stat"
    upath <- tryGetUniPath mtpath "path"
    scriptIO $ loggerIO debug $ "path: " ++ (show upath)
    item  <- scriptIO $ BatchFS.stat upath
    return $ encode item


mkdir :: IORef Batch -> Maybe Text -> IO ()
mkdir _ mtpath = tRunScript $ do
    scriptIO $ loggerIO info "called mkdir"
    upath <- tryGetUniPath mtpath "path"
    scriptIO $ loggerIO debug $ "path: " ++ (show upath)
    scriptIO $ BatchFS.mkdir upath


touch :: IORef Batch -> Maybe Text -> IO ()
touch _ mtpath = tRunScript $ do
    scriptIO $ loggerIO info "called touch"
    upath <- tryGetUniPath mtpath "path"
    scriptIO $ loggerIO debug $ "path: " ++ (show upath)
    scriptIO $ BatchFS.touch upath


rm :: IORef Batch -> Maybe Text -> IO ()
rm _ mtpath = tRunScript $ do
    scriptIO $ loggerIO info "called rm"
    upath <- tryGetUniPath mtpath "path"
    scriptIO $ loggerIO debug $ "path: " ++ (show upath)
    scriptIO $ BatchFS.rm upath


cp :: IORef Batch -> Maybe Text -> Maybe Text -> IO ()
cp _ mtsrc mtdst = tRunScript $ do
    scriptIO $ loggerIO info "called cp"
    usrc <- tryGetUniPath mtsrc "src"
    udst <- tryGetUniPath mtdst "dst"
    scriptIO $ loggerIO debug $ "src: " ++ (show usrc) ++ " dst: " ++ (show udst)
    scriptIO $ BatchFS.cp usrc udst


mv :: IORef Batch -> Maybe Text -> Maybe Text -> IO ()
mv _ mtsrc mtdst = tRunScript $ do
    scriptIO $ loggerIO info "called mv"
    usrc <- tryGetUniPath mtsrc "src"
    udst <- tryGetUniPath mtdst "dst"
    scriptIO $ loggerIO debug $ "src: " ++ (show usrc) ++ " dst: " ++ (show udst)
    scriptIO $ BatchFS.mv usrc udst
