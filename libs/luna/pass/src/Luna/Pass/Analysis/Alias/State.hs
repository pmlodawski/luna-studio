---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

module Luna.Pass.Analysis.Alias.State where

import           Control.Monad.State (MonadState, get, modify, put)
import qualified Data.IntMap         as IntMap

import           Flowbox.Prelude           hiding (id)
import           Flowbox.System.Log.Logger
import           Luna.AST.AST              (AST, ID)
import qualified Luna.AST.AST              as AST
import           Luna.AST.Expr             (Expr)
import qualified Luna.AST.Expr             as Expr
import           Luna.AST.Lit              (Lit)
import qualified Luna.AST.Lit              as Lit
import           Luna.AST.Module           (Module)
import qualified Luna.AST.Module           as Module
import           Luna.AST.Pat              (Pat)
import qualified Luna.AST.Pat              as Pat
import           Luna.AST.Type             (Type)
import qualified Luna.AST.Type             as Type
import           Luna.Data.AliasInfo       (AliasInfo)
import qualified Luna.Data.AliasInfo       as AliasInfo



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
    withJust mcid (\cid -> modifyAliasInfo $ AliasInfo.parent %~ IntMap.insert id cid)


registerAST :: VAMonad m => ID -> AST -> m ()
registerAST id ast = modifyAliasInfo $ AliasInfo.ast %~ IntMap.insert id ast


regVarName :: VAMonad m => String -> ID -> m ()
regVarName = regName AliasInfo.varnames

regTypeName :: VAMonad m => String -> ID -> m ()
regTypeName = regName AliasInfo.typenames


regName lens name id = do
    a    <- getAliasInfo
    mcid <- getCurrentID
    case mcid of
        Nothing  -> fail "Unable to get current id"
        Just cid -> putAliasInfo a2
            where varRel  = a ^. (AliasInfo.scope . (ix cid))
                  varRel2 = varRel & lens.at name ?~ id
                  a2      = a & AliasInfo.scope.at cid ?~ varRel2


regParentVarName :: VAMonad m => String -> ID -> m ()
regParentVarName = withParentID .: regVarName


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
    where dstIDLookup          = varnames ^. at name
          mPid                 = (a ^. AliasInfo.parent) ^. at ctxID
          varRel               = a ^. AliasInfo.scope.ix ctxID
          varnames             = varRel ^. AliasInfo.varnames
          updateAliasMap val   = a & AliasInfo.alias.at id ?~ val
          updateInvalidMap val = a & AliasInfo.orphans.at id ?~ val



------------------------------------------------------------------------
-- Instances
------------------------------------------------------------------------

instance Monoid VAState where
    mempty      = VAState mempty mempty
    mappend a b = VAState (mappend (a ^. aa)      (b ^. aa))
                          (mappend (a ^. idStack) (b ^. idStack))
