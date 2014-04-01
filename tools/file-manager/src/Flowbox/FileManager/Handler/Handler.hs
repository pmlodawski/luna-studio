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
import           Flowbox.Bus.Data.Topic                      (Topic)
import qualified Flowbox.FileManager.Handler.FileSystem as FSHandler
import           Flowbox.Prelude                        hiding (error)
import           Flowbox.System.Log.Logger



logger :: LoggerIO
logger = getLoggerIO "Flowbox.FileManager.Handler"


topics :: [Topic]
topics = [ "filesystem.ls.request"
         , "filesystem.stat.request"
         , "filesystem.mkdir.request"
         , "filesystem.touch.request"
         , "filesystem.rm.request"
         , "filesystem.cp.request"
         ]

mkList :: a -> [a]
mkList a = [a]

singleResult f a = mkList <$> f a


handler :: BusRPCHandler
handler callback topic = case topic of
    "filesystem.ls.request"    -> callback P.update $ singleResult FSHandler.ls
    "filesystem.stat.request"  -> callback P.update $ singleResult FSHandler.stat
    "filesystem.mkdir.request" -> callback P.status $ singleResult FSHandler.mkdir
    "filesystem.touch.request" -> callback P.status $ singleResult FSHandler.touch
    "filesystem.rm.request"    -> callback P.update $ singleResult FSHandler.rm
    "filesystem.cp.request"    -> callback P.update $ singleResult FSHandler.cp
    unsupported             -> do let errMsg = "Unknown topic: " ++ show unsupported
                                  logger error errMsg
                                  return $ P.respondError topic errMsg
