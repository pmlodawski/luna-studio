---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

{-# LANGUAGE NoMonomorphismRestriction #-}
module Luna.Pass.Pass where

import           Flowbox.Prelude
import qualified Data.HMap as HMap
import           Data.HMap (HMap)
import           Control.Monad.Trans.Either (left, right)
import           Data.TypeLevel.List        (app)
import qualified Luna.Pass.Datastore as Datastore
import           Luna.Pass.Datastore (Datastore)
import           Luna.Pass.Data (DataInfo)


----------------------------------------------------------------------
-- Data types
----------------------------------------------------------------------

data Morph ins outs f = Morph ins outs f


runMorph (Morph ins outs f) dataStore = case args of 
    Left  desc -> left $ MissingData desc
    Right args -> do
        results <- app f args
        right $ Datastore.inserts outs results dataStore
    where args = Datastore.lookups ins dataStore


data Pass m = Pass {
                 _morph :: Datastore -> m Datastore
}

pass = Pass . runMorph



runPasses :: Monad m => [Pass m] -> Datastore -> m Datastore
runPasses ps dataStore = foldl (>>=) (return dataStore) (fmap _morph ps)



data PassError = MissingData [DataInfo]
               deriving (Show)