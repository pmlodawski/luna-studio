---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.System.Log.Conf where

import Control.Concurrent.MVar
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Map as Map


data Conf = Conf { indent :: Int
                 } deriving (Show)


type LogConf = Map.Map String Conf

empty :: Conf
empty = Conf 0

lempty :: LogConf
lempty = Map.empty


conf :: MVar LogConf
conf = unsafePerformIO $ newMVar lempty


read :: String -> IO Conf
read lname = modifyMVar conf $ \c -> 
    case Map.lookup lname c of
        Just x  -> return (c,x)
        Nothing -> do
            let x  = empty
                nc = Map.insert lname x c
            return (nc,x)


store :: String -> Conf -> IO ()
store lname nc = modifyMVar conf $ \c -> 
    return (Map.insert lname nc c, ())
