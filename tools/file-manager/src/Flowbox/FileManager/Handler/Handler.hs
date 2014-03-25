---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes      #-}

module Flowbox.FileManager.Handler.Handler where

import           Flowbox.Bus.RPC.BusRPCHandler          (BusRPCHandler)
import qualified Flowbox.Bus.RPC.Processor              as P
import           Flowbox.Bus.Topic                      (Topic)
import qualified Flowbox.FileManager.Handler.FileSystem as FSHandler
import           Flowbox.Prelude                        hiding (error)
import           Flowbox.System.Log.Logger



logger :: LoggerIO
logger = getLoggerIO "Flowbox.FileManager.Processor"


topics :: [Topic]
topics = [ "filesystem.ls.request"   
         , "filesystem.stat.request" 
         , "filesystem.mkdir.request"
         , "filesystem.touch.request"
         , "filesystem.rm.request"   
         , "filesystem.cp.request"
         ]


handler :: BusRPCHandler
handler  callback topic = case topic of
    "filesystem.ls.request"    -> callback P.update FSHandler.ls
    "filesystem.stat.request"  -> callback P.update FSHandler.stat
    "filesystem.mkdir.request" -> callback P.status FSHandler.mkdir
    "filesystem.touch.request" -> callback P.status FSHandler.touch
    "filesystem.rm.request"    -> callback P.update FSHandler.rm
    "filesystem.cp.request"    -> callback P.update FSHandler.cp
    unsupported             -> do let errMsg = "Unknown topic: " ++ show unsupported
                                  logger error errMsg
                                  return $ P.respondError topic errMsg
