{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverlappingInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  System.Log.Data
-- Copyright   :  (C) 2015 Flowbox
-- License     :  Apache-2.0
-- Maintainer  :  Wojciech Daniło <wojciech.danilo@gmail.com>
-- Stability   :  stable
-- Portability :  portable
-----------------------------------------------------------------------------

module System.Log.Data where

import Prelude                hiding (lookup)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Concurrent     (threadDelay)
import System.Log.Log         (Log, fromLog)


----------------------------------------------------------------------
-- Basic data wrappers
----------------------------------------------------------------------

data Data base = Data { recBase :: base
                      , recData :: DataOf base
                      }

class DataGetter base m where
    getData :: m (Data base)

type family DataOf a :: *

deriving instance (Show (DataOf base), Show base) => Show (Data base)

----------------------------------------------------------------------
-- RecordBuilder
----------------------------------------------------------------------

newtype RecordBuilder a = RecordBuilder { fromRecordBuilder :: a } deriving (Show, Functor)
empty = RecordBuilder ()

appData :: (a~DataOf base) => base -> a -> RecordBuilder as -> RecordBuilder (Data base, as)
appData base a = fmap (Data base a,)


----------------------------------------------------------------------
-- Data reading
----------------------------------------------------------------------

class LookupDataSet base s where 
    lookupDataSet :: base -> s -> Data base

instance LookupDataSet base (Data base,as) where
    lookupDataSet _ (a,_) = a

instance LookupDataSet base as => LookupDataSet base (Data b,as) where
    lookupDataSet b (_, as) = lookupDataSet b as



class Lookup base s where 
    lookup :: base -> s -> Data base

instance LookupDataSet base l => Lookup base (Log l) where
    lookup b (fromLog -> s) = lookupDataSet b s

instance LookupDataSet base r => Lookup base (RecordBuilder r) where
    lookup b (fromRecordBuilder -> r) = lookupDataSet b r


readData :: Lookup a l => a -> l -> DataOf a
readData a = recData . lookup a



----------------------------------------------------------------------
-- Simple data providers
----------------------------------------------------------------------

-- Time --

data Time = Time deriving (Show)
type instance DataOf Time = Int

instance MonadIO m => DataGetter Time m where
    getData = do liftIO $ print "reading time"
                 liftIO $ threadDelay 1000000
                 return $ Data Time 5

-- Msg --

data Msg = Msg deriving (Show)
type instance DataOf Msg = String


-- Lvl -- 

data Lvl = Lvl deriving (Show)
type instance DataOf Lvl = LevelData
data LevelData = LevelData Int String deriving (Show, Ord, Eq)
mkLevel a = LevelData (fromEnum a) (show a)