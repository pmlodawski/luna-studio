---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}

module Luna.Pass.Datastore where

import           Flowbox.Prelude hiding (lookup)
import qualified Data.HMap       as HMap
import           Data.HMap       (HMap)
import qualified Data.HTSet.HTSet as HTSet
import           Data.HTSet.HTSet (HTSet)
import           Luna.Pass.Data  (PassData(PassData), DataInfo(DataInfo))
import qualified Luna.Pass.Data  as Data
import           Data.Typeable   (Typeable)


----------------------------------------------------------------------
-- Data types
----------------------------------------------------------------------

type DatastoreLookup a = Either [DataInfo] a

newtype Datastore = Datastore HTSet

registerData :: String -> String -> PassData a
registerData name desc = PassData (DataInfo name desc)


insertKey :: Typeable a => PassData a -> a -> Datastore -> Datastore
insertKey k val (Datastore m) = Datastore $ HTSet.insert val m

lookupKey :: Typeable a => PassData a -> Datastore -> Maybe a
lookupKey k (Datastore m) = HTSet.lookup m


instance Show Datastore where
    show _ = "Datastore"

instance Monoid Datastore where
    mempty  = Datastore $ mempty
    mappend (Datastore a) (Datastore b) = Datastore $ mappend a b


class Lookups keys a | keys -> a where
    lookups :: keys -> Datastore -> DatastoreLookup a

instance Lookups () () where
    lookups _ _ = Right ()

instance (Lookups ks vs, Typeable v) => Lookups (PassData v,ks) (v,vs) where
    lookups (k,ks) dict = case (lookups ks dict) of
        Right lst -> case dstVal of
            Just a  -> Right (a,lst)
            Nothing -> Left  [view Data.info k]
        Left orphans -> case dstVal of
            Just _  -> Left orphans
            Nothing -> Left $ view Data.info k : orphans
        where dstVal = lookupKey k dict


class Inserts keys vals where
    inserts :: keys -> vals -> Datastore -> Datastore

instance (vals~()) => Inserts () vals where
    inserts _ _ = id

instance (vals~(v,vs), k ~ PassData v, Inserts ks vs, Typeable v) => Inserts (k,ks) vals where
    inserts (k,ks) (v,vs) dataStore = inserts ks vs $ insertKey k v dataStore