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
logger = getLogger "Flowbox.Luna.Passes.VarAlias.State"


data VAState = VAState { _aa      :: AliasInfo
                       , _idStack :: [ID]
                       }
             deriving (Show)

makeLenses (''VAState)

type VAMonad m = (MonadState VAState m, Applicative m)


getAA :: VAMonad m => m AliasInfo
getAA = view aa <$> get

getCurrentID :: VAMonad m => m (Maybe ID)
getCurrentID = do stack <- view idStack <$> get
                  return $ case stack of
                      []    -> Nothing
                      (x:_) -> Just x


putAA :: VAMonad m => AliasInfo -> m ()
putAA naa = modify (aa .~ naa)


modifyAA :: VAMonad m => (AliasInfo -> AliasInfo) -> m ()
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

registerModule :: VAMonad m => Module -> m ()
registerModule el = registerAST id (AST.Module el) *> registerID id
    where id = (el ^. Module.id)

registerExpr :: VAMonad m => Expr -> m ()
registerExpr el = registerAST id (AST.Expr el) *> registerID id
    where id = (el ^. Expr.id)

registerLit :: VAMonad m => Lit -> m ()
registerLit el = registerAST id (AST.Lit el) *> registerID id
    where id = (el ^. Lit.id)

registerPat :: VAMonad m => Pat -> m ()
registerPat el = registerAST id (AST.Pat el) *> registerID id
    where id = (el ^. Pat.id)

registerType :: VAMonad m => Type -> m ()
registerType el = registerAST id (AST.Type el) *> registerID id
    where id = (el ^. Type.id)


registerID :: VAMonad m => ID -> m ()
registerID id = do
    mcid <- getCurrentID
    withJust mcid (\cid -> modifyAA $ AliasInfo.parentMap %~ IntMap.insert id cid)


registerAST :: VAMonad m => ID -> AST -> m ()
registerAST id ast = modifyAA $ AliasInfo.astMap %~ IntMap.insert id ast


registerVarName :: VAMonad m => String -> ID -> m ()
registerVarName name id = do
    a    <- getAA
    mcid <- getCurrentID
    case mcid of
        Nothing  -> return ()
        Just cid -> putAA a2
            where varRel  = a ^. (AliasInfo.varRel . (ix cid))
                  varRel2 = varRel & AliasInfo.nameMap.at name ?~ id
                  a2      = a & AliasInfo.varRel.at cid ?~ varRel2



bindVar :: VAMonad m => ID -> String -> m ()
bindVar id name = do
    mcid <- getCurrentID
    withJust mcid (\cid -> modifyAA (bindVarRec id cid name))


bindVarRec :: ID -> ID -> String -> AliasInfo -> AliasInfo
bindVarRec id ctxID name a = case dstIDLookup of
    Just dstID -> updateAliasMap $ Right dstID
    Nothing    -> case mPid of
                  Just pid -> bindVarRec id pid name a
                  Nothing  -> updateAliasMap $ Left (AliasInfo.LookupError name)
    where dstIDLookup        = nameMap ^. at name
          mPid               = (a ^. AliasInfo.parentMap) ^. at ctxID
          varRel             = a ^. AliasInfo.varRel.ix ctxID
          nameMap            = varRel ^. AliasInfo.nameMap
          updateAliasMap val = a & AliasInfo.aliasMap.at id ?~ val



------------------------------------------------------------------------
-- Instances
------------------------------------------------------------------------

instance Monoid VAState where
    mempty      = VAState mempty mempty
    mappend a b = VAState (mappend (a ^. aa)      (b ^. aa))
                          (mappend (a ^. idStack) (b ^. idStack))
