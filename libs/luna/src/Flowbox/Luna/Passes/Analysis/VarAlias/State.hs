---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Flowbox.Luna.Passes.Analysis.VarAlias.State where

import qualified Control.Monad.State as State
import           Control.Monad.State (MonadState, get, put, modify)
import qualified Data.IntMap         as IntMap
import           Data.Map            (Map)
import qualified Data.Map            as Map

import           Flowbox.Luna.Data.Analysis.Alias.Alias (AA, ID)
import qualified Flowbox.Luna.Data.Analysis.Alias.Alias as AA
import           Flowbox.Prelude                                hiding (id)
import           Flowbox.System.Log.Logger 

import qualified Debug.Trace as Debug

logger :: Logger
logger = getLogger "Flowbox.Luna.Passes.VarAlias.State"


data VAState = VAState { _aa        :: AA
                       , _currentID :: ID
                       }
             deriving (Show)

makeLenses (''VAState)


instance Monoid VAState where
    mempty = VAState mempty 0


type VAMonad m = (MonadState VAState m, Functor m)


--withState f = do
--    st <- get
--    put $ f st


getAA :: VAMonad m => m AA
getAA = view aa <$> get

getCurrentID :: VAMonad m => m ID
getCurrentID = view currentID <$> get


putAA :: VAMonad m => AA -> m ()
putAA naa = modify (aa .~ naa)


modifyAA f = do
    aa <- getAA
    putAA $ f aa


switchID :: VAMonad m => ID -> m ()
switchID id = modify (currentID .~ id)


--bind :: VAMonad m => ID -> Either AA.Error ID -> m ()
--bind srcId dst = do
--    aa <- get
--    put $ aa & AA.aliasMap %~ (IntMap.insert srcId dst)
--    --aa { AA.varmap = IntMap.insert srcId dst $ AA.varmap aa }



----updateVarStat :: VAMonad m => AA -> m ()
----updateVarStat ns = do
----    let nvs = varstat ns
----    s <- get
----    put $ s { varstat = nvs }


--registerVarName :: VAMonad m => String -> ID -> m ()
--registerVarName name id = do
--    st <- get
--    let a       = (st ^. aa)
--        cid     = (st ^. currentID)
--        varRel  = IntMap.findWithDefault mempty cid (a ^. AA.varRel)
--        varRel2 = varRel & AA.nameMap %~ Map.insert name id
--        a2      = a      & AA.varRel  %~ IntMap.insert cid varRel2
--        st2     = st     & aa         .~ a2
--    put st2

registerID :: VAMonad m => ID -> m ()
registerID id = do
    cid <- getCurrentID
    modifyAA $ AA.parentMap %~ IntMap.insert id cid


registerVarName :: VAMonad m => String -> ID -> m ()
registerVarName name id = do
    a   <- getAA
    cid <- getCurrentID
    let varRel  = a ^. (AA.varRel . (ix cid))
        varRel2 = varRel & AA.nameMap.at name ?~ id
        a2      = a & AA.varRel.at cid ?~ varRel2
    putAA a2


bindVar :: VAMonad m => ID -> String -> m ()
bindVar id name = do
    cid <- getCurrentID
    modifyAA (bindVarRec cid name)


bindVarRec :: ID -> String -> AA -> AA
bindVarRec ctxID name a = case dstIDLookup of
    Just dstID -> updateAliasMap $ Right dstID
    Nothing    -> case mPid of
                  Just pid -> bindVarRec pid name a
                  Nothing  -> updateAliasMap $ Left (AA.LookupError name)
    where dstIDLookup        = nameMap ^. at name
          mPid               = (a ^. AA.parentMap) ^. at ctxID
          varRel             = a ^. AA.varRel.ix ctxID
          nameMap            = varRel ^. AA.nameMap
          updateAliasMap val = a & AA.aliasMap.at ctxID ?~ val




        --Just pid = (a ^. AA.parentMap) ^. at cid
        --pVarRel  = a ^. AA.varRel.ix pid
        --nameMap  = pVarRel ^. AA.nameMap
        --na       = a & case nameMap ^. at name of
        --               Just dstID -> AA.aliasMap.at cid ?~ Right dstID
        --               Nothing    -> AA.aliasMap.at cid ?~ Left (AA.LookupError name)

--lookupVar :: VAMonad m => String -> m (Maybe ID)
--lookupVar name = do
--    s <- get
--    return $ Map.lookup name (view AA.nameMap s)


--bindVar :: VAMonad m => ID -> String -> m ()
--bindVar id name = do target <- lookupVar name
--                     bind id $ case target of
--                        Just tid -> Right tid
--                        Nothing  -> Left $ AA.LookupError name
