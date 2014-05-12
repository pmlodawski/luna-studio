---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}

module Flowbox.AWS.EC2.Instance.Pool where

import           Control.Concurrent.MVar (MVar)
import qualified Control.Concurrent.MVar as MVar
import           Control.Monad.IO.Class  (liftIO)
import           Data.Map                (Map)
import qualified Data.Map                as Map
import           Data.Set                (Set)
import qualified Data.Set                as Set
import qualified Data.Tuple as Tuple

import           Flowbox.AWS.EC2.EC2               (EC2, EC2Resource)
import qualified Flowbox.AWS.EC2.Instance.Instance as Instance
import qualified Flowbox.AWS.User.User             as User
import           Flowbox.Prelude
import           Flowbox.System.Log.Logger



logger :: LoggerIO
logger = getLoggerIO "Flowbox.AWS.Instance.Pool"



data Pool = Pool { used :: Map Instance.ID User.Name
                 , free :: Set Instance.ID
                 } deriving (Show, Read)


type MPool = MVar Pool



getUsed :: User.Name -> Pool -> Maybe Instance.ID
getUsed userName pool = Map.lookup userName $ Map.fromList $ map Tuple.swap $ Map.toList $ used pool


takeFree :: Pool -> (Pool, Maybe Instance.ID)
takeFree pool = let
    f = free pool
    in if Set.null f
            then (pool, Nothing)
            else -- FIXME [PM] : ugly method, because of dependency containers==0.5.0.0
                 --let newpool = pool { free = Set.deleteAt 0 f}
                 --in (newpool, Just $ Set.elemAt 0 f)
                 let freeElem = head $ Set.toList f
                     newpool = pool { free = Set.delete freeElem f}
                 in (newpool, Just freeElem)


makeFree :: Instance.ID -> Pool -> Pool
makeFree instanceID pool =
    pool { used = Map.delete instanceID $ used pool
         , free = Set.insert instanceID $ free pool
         }





initialize :: EC2Resource m => EC2 m ()
initialize = undefined


release :: EC2Resource m => Instance.ID -> MPool -> EC2 m ()
release instanceID mpool = do
    liftIO $ MVar.modifyMVar_ mpool (return . makeFree instanceID)
    Instance.tagWithUser Nothing [instanceID]
    freeUnused mpool


--data Status = Used Instance.ID
--            | Free Instance.ID
--            | 

retrieve :: EC2Resource m => User.Name -> MPool -> EC2 m Instance.ID
retrieve userName mpool = do
    minstanceID <- liftIO $ MVar.modifyMVar mpool (return . takeFree)
    case minstanceID of
        Just instanceID -> do Instance.tagWithUser (Just userName) [instanceID]
                              Instance.prepareForNewUser userName instanceID
                              return instanceID

freeUnused :: EC2Resource m => MPool -> EC2 m ()
freeUnused = undefined


monitor :: EC2Resource m => MPool -> EC2 m ()
monitor = undefined
