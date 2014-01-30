---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

module Flowbox.Luna.Passes.Analysis.Alias.State where

import           Control.Monad.State (MonadState, get, put, modify)
import qualified Data.IntMap         as IntMap

import           Flowbox.Luna.Data.AST.AST        (AST, ID)
import qualified Flowbox.Luna.Data.AST.AST        as AST
import           Flowbox.Luna.Data.AST.Expr       (Expr)
import qualified Flowbox.Luna.Data.AST.Expr       as Expr
import           Flowbox.Luna.Data.AST.Lit        (Lit)
import qualified Flowbox.Luna.Data.AST.Lit        as Lit
import           Flowbox.Luna.Data.AST.Module     (Module)
import qualified Flowbox.Luna.Data.AST.Module     as Module
import           Flowbox.Luna.Data.AST.Pat        (Pat)
import qualified Flowbox.Luna.Data.AST.Pat        as Pat
import           Flowbox.Luna.Data.AST.Type       (Type)
import qualified Flowbox.Luna.Data.AST.Type       as Type
import           Flowbox.Luna.Data.Pass.AliasInfo (AliasInfo)
import qualified Flowbox.Luna.Data.Pass.AliasInfo as AliasInfo
import           Flowbox.Prelude                  hiding (id)
import           Flowbox.System.Log.Logger



logger :: Logger
logger = getLogger "Flowbox.Luna.Passes.Alias.State"


data VAState = VAState { _aa      :: AliasInfo
                       , _idStack :: [ID]
                       }
             deriving (Show)

makeLenses (''VAState)

type VAMonad m = (MonadState VAState m, Applicative m)


getAliasInfo :: VAMonad m => m AliasInfo
getAliasInfo = view aa <$> get

getCurrentID :: VAMonad m => m (Maybe ID)
getCurrentID = do stack <- view idStack <$> get
                  return $ case stack of
                      []    -> Nothing
                      (x:_) -> Just x


putAliasInfo :: VAMonad m => AliasInfo -> m ()
putAliasInfo naa = modify (aa .~ naa)


modifyAliasInfo :: VAMonad m => (AliasInfo -> AliasInfo) -> m ()
modifyAliasInfo f = do
    aa' <- getAliasInfo
    putAliasInfo $ f aa'


pushID :: VAMonad m => ID -> m ()
pushID id = modify (idStack %~ (id:))

popID :: VAMonad m => m ID
popID = do (id:ids) <- view idStack <$> get
           modify (idStack .~ ids)
           return id


withID :: VAMonad m => ID -> m f -> m f
withID id f = pushID id *> f <* popID


withParentID :: VAMonad m => m f -> m f
withParentID f = do pid <- popID
                    out <- f
                    pushID pid
                    return out

--switchID :: VAMonad m => ID -> m ()
--switchID id = modify (currentID .~ id)

registerModule :: VAMonad m => Module -> m ()
registerModule = registerElBy AST.Module Module.id

registerExpr :: VAMonad m => Expr -> m ()
registerExpr = registerElBy AST.Expr Expr.id

registerLit :: VAMonad m => Lit -> m ()
registerLit = registerElBy AST.Lit Lit.id

registerPat :: VAMonad m => Pat -> m ()
registerPat = registerElBy AST.Pat Pat.id

registerType :: VAMonad m => Type -> m ()
registerType = registerElBy AST.Type Type.id


registerElBy fCon fID el = registerAST id (fCon el) *> registerID id
    where id = (el ^. fID)


registerID :: VAMonad m => ID -> m ()
registerID id = do
    mcid <- getCurrentID
    withJust mcid (\cid -> modifyAliasInfo $ AliasInfo.parentMap %~ IntMap.insert id cid)


registerAST :: VAMonad m => ID -> AST -> m ()
registerAST id ast = modifyAliasInfo $ AliasInfo.astMap %~ IntMap.insert id ast


registerName :: VAMonad m => String -> ID -> m ()
registerName name id = do
    a    <- getAliasInfo
    mcid <- getCurrentID
    case mcid of
        Nothing  -> fail "Unable to get current id"
        Just cid -> putAliasInfo a2
            where varRel  = a ^. (AliasInfo.varRel . (ix cid))
                  varRel2 = varRel & AliasInfo.nameMap.at name ?~ id
                  a2      = a & AliasInfo.varRel.at cid ?~ varRel2


registerParentName :: VAMonad m => String -> ID -> m ()
registerParentName = withParentID .: registerName 



bindVar :: VAMonad m => ID -> String -> m ()
bindVar id name = do
    mcid <- getCurrentID
    withJust mcid (\cid -> modifyAliasInfo (bindVarRec id cid name))


bindVarRec :: ID -> ID -> String -> AliasInfo -> AliasInfo
bindVarRec id ctxID name a = case dstIDLookup of
    Just dstID -> updateAliasMap dstID
    Nothing    -> case mPid of
                  Just pid -> bindVarRec id pid name a
                  Nothing  -> updateInvalidMap $ AliasInfo.LookupError name
    where dstIDLookup          = nameMap ^. at name
          mPid                 = (a ^. AliasInfo.parentMap) ^. at ctxID
          varRel               = a ^. AliasInfo.varRel.ix ctxID
          nameMap              = varRel ^. AliasInfo.nameMap
          updateAliasMap val   = a & AliasInfo.aliasMap.at id ?~ val
          updateInvalidMap val = a & AliasInfo.invalidMap.at id ?~ val



------------------------------------------------------------------------
-- Instances
------------------------------------------------------------------------

instance Monoid VAState where
    mempty      = VAState mempty mempty
    mappend a b = VAState (mappend (a ^. aa)      (b ^. aa))
                          (mappend (a ^. idStack) (b ^. idStack))
