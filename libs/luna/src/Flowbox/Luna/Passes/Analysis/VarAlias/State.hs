---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

module Flowbox.Luna.Passes.Analysis.VarAlias.State where

import           Control.Monad.State (MonadState, get, modify)
import qualified Data.IntMap         as IntMap

import           Flowbox.Luna.Data.Analysis.Alias.Alias (AA, ID)
import qualified Flowbox.Luna.Data.Analysis.Alias.Alias as AA
import           Flowbox.Prelude                        hiding (id)
import           Flowbox.System.Log.Logger



logger :: Logger
logger = getLogger "Flowbox.Luna.Passes.VarAlias.State"


data VAState = VAState { _aa      :: AA
                       , _idStack :: [ID]
                       }
             deriving (Show)

makeLenses (''VAState)

type VAMonad m = (MonadState VAState m, Applicative m)


getAA :: VAMonad m => m AA
getAA = view aa <$> get

getCurrentID :: VAMonad m => m ID
getCurrentID = head . view idStack <$> get


putAA :: VAMonad m => AA -> m ()
putAA naa = modify (aa .~ naa)


modifyAA :: VAMonad m => (AA -> AA) -> m ()
modifyAA f = do
    aa' <- getAA
    putAA $ f aa'


pushID :: VAMonad m => ID -> m ()
pushID id = modify (idStack %~ (id:))

popID :: VAMonad m => m ()
popID = modify (idStack %~ tail)


withID :: VAMonad m => ID -> m f -> m f
withID id f = pushID id *> f <* popID

--switchID :: VAMonad m => ID -> m ()
--switchID id = modify (currentID .~ id)


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
    modifyAA (bindVarRec id cid name)


bindVarRec :: ID -> ID -> String -> AA -> AA
bindVarRec id ctxID name a = case dstIDLookup of
    Just dstID -> updateAliasMap $ Right dstID
    Nothing    -> case mPid of
                  Just pid -> bindVarRec id pid name a
                  Nothing  -> updateAliasMap $ Left (AA.LookupError name)
    where dstIDLookup        = nameMap ^. at name
          mPid               = (a ^. AA.parentMap) ^. at ctxID
          varRel             = a ^. AA.varRel.ix ctxID
          nameMap            = varRel ^. AA.nameMap
          updateAliasMap val = a & AA.aliasMap.at id ?~ val



------------------------------------------------------------------------
-- Instances
------------------------------------------------------------------------

instance Monoid VAState where
    mempty      = VAState mempty mempty
    mappend a b = VAState (mappend (a ^. aa)      (b ^. aa))
                          (mappend (a ^. idStack) (b ^. idStack))
