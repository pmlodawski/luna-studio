---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}

module Flowbox.Luna.Passes.Analysis.VarAlias.State where

import           Control.Monad.State
import qualified Data.IntMap         as IntMap
import           Data.Map            (Map)
import qualified Data.Map            as Map

import           Flowbox.Luna.Data.Analysis.Alias.Alias (AA, ID)
import qualified Flowbox.Luna.Data.Analysis.Alias.Alias as AA
import           Flowbox.Prelude                                hiding (id)
import           Flowbox.System.Log.Logger



logger :: Logger
logger = getLogger "Flowbox.Luna.Passes.VarAlias.State"



type AAState m = MonadState AA m


bind :: AAState m => ID -> Either AA.Error ID -> m ()
bind srcId dst = do
    aa <- get
    put $ aa & AA.aliasMap %~ (IntMap.insert srcId dst)
    --aa { AA.varmap = IntMap.insert srcId dst $ AA.varmap aa }



--updateVarStat :: AAState m => AA -> m ()
--updateVarStat ns = do
--    let nvs = varstat ns
--    s <- get
--    put $ s { varstat = nvs }


registerVarName :: AAState m => (String, ID) -> m ()
registerVarName (name, id) = do
    aa <- get
    put $ aa & AA.nameMap %~ (Map.insert name id)


lookupVar :: AAState m => String -> m (Maybe ID)
lookupVar name = do
    s <- get
    return $ Map.lookup name (view AA.nameMap s)


bindVar :: AAState m => ID -> String -> m ()
bindVar id name = do target <- lookupVar name
                     bind id $ case target of
                        Just tid -> Right tid
                        Nothing  -> Left $ AA.LookupError name
