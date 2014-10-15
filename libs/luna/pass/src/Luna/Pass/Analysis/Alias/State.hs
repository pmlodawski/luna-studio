---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}

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
import           Luna.Data.Namespace       (Namespace)
import qualified Luna.Data.Namespace       as Namespace


logger :: Logger
logger = getLogger "Flowbox.Luna.Passes.Alias.State"


--data VAState = VAState { _info    :: AliasInfo
--                       , _idStack :: [ID]
--                       }
--             deriving (Show)

--makeLenses (''VAState)

type VAMonad m = (MonadState Namespace m, Applicative m)


getAliasInfo :: VAMonad m => m AliasInfo
getAliasInfo = view Namespace.info <$> get

--getCurrentID :: VAMonad m => m (Maybe ID)
--getCurrentID = do stack <- view idStack <$> get
--                  return $ case stack of
--                      []    -> Nothing
--                      (x:_) -> Just x


scopeID = Namespace.head <$> get


putAliasInfo :: VAMonad m => AliasInfo -> m ()
putAliasInfo info = modify (Namespace.info .~ info)


modifyAliasInfo :: VAMonad m => (AliasInfo -> AliasInfo) -> m ()
modifyAliasInfo f = do
    info <- getAliasInfo
    putAliasInfo $ f info



--pushID :: VAMonad m => ID -> m ()
pushID id = modify $ Namespace.pushID id

--popID :: VAMonad m => m ID
popID = do
    (id, ns') <- Namespace.popID <$> get
    put ns'
    return id



--pushID :: VAMonad m => ID -> m ()
--pushID id = modify (idStack %~ (id:))

--popID :: VAMonad m => m ID
--popID = do (id:ids) <- view idStack <$> get
--           modify (idStack .~ ids)
--           return id


--withID :: VAMonad m => ID -> m f -> m f
--withID id f = pushID id *> f <* popID

pushScope id = modify $ Namespace.pushScope id

popScope = modify $ Namespace.popScope

withScope id p = pushScope id *> p <* popScope


withParentID :: VAMonad m => m f -> m f
withParentID f = do pid <- popID
                    out <- f
                    pushID pid
                    return out

----switchID :: VAMonad m => ID -> m ()
----switchID id = modify (currentID .~ id)

regModule :: VAMonad m => Module -> m ()
regModule = registerElBy AST.Module Module.id

regExpr :: VAMonad m => Expr -> m ()
regExpr = registerElBy AST.Expr Expr.id

regLit :: VAMonad m => Lit -> m ()
regLit = registerElBy AST.Lit Lit.id

regPat :: VAMonad m => Pat -> m ()
regPat = registerElBy AST.Pat Pat.id

regType :: VAMonad m => Type -> m ()
regType = registerElBy AST.Type Type.id


registerElBy fCon fID el = regAST id (fCon el) *> registerID id
    where id = el ^. fID


registerID :: VAMonad m => ID -> m ()
registerID id = do
    mpid <- scopeID
    withJust mpid (\pid -> modifyAliasInfo $ AliasInfo.parent %~ IntMap.insert id pid)


--registerAST :: VAMonad m => ID -> AST -> m ()
regAST id ast = modifyAliasInfo $ AliasInfo.regAST id ast


regVarName :: VAMonad m => String -> ID -> m ()
regVarName = regName AliasInfo.varnames

regTypeName :: VAMonad m => String -> ID -> m ()
regTypeName = regName AliasInfo.typenames


regName lens name id = do
    a    <- getAliasInfo
    mcid <- scopeID
    case mcid of
        Nothing  -> fail "Unable to get current id"
        Just cid -> putAliasInfo a2
            where varRel  = a ^. (AliasInfo.scope . (ix cid))
                  varRel2 = varRel & lens.at name ?~ id
                  a2      = a & AliasInfo.scope.at cid ?~ varRel2


regParentVarName :: VAMonad m => String -> ID -> m ()
regParentVarName = withParentID .: regVarName


bindVar id name = do
    ns <- get
    case Namespace.bindVar id name ns of
        Left _    -> fail $ "Unable to bind variable " ++ name -- FIXME[wd]: nicer error messages
        Right ns' -> put ns'


--bindVar :: VAMonad m => ID -> String -> m ()
--bindVar id name = do
--    mcid <- getCurrentID
--    withJust mcid (\cid -> modifyAliasInfo (bindVarRec id cid name))


--bindVarRec :: ID -> ID -> String -> AliasInfo -> AliasInfo
--bindVarRec id ctxID name a = case dstIDLookup of
--    Just dstID -> updateAliasMap dstID
--    Nothing    -> case mPid of
--                  Just pid -> bindVarRec id pid name a
--                  Nothing  -> updateInvalidMap $ AliasInfo.LookupError name
--    where dstIDLookup          = varnames ^. at name
--          mPid                 = (a ^. AliasInfo.parent) ^. at ctxID
--          varRel               = a ^. AliasInfo.scope.ix ctxID
--          varnames             = varRel ^. AliasInfo.varnames
--          updateAliasMap val   = a & AliasInfo.alias.at id ?~ val
--          updateInvalidMap val = a & AliasInfo.orphans.at id ?~ val



--------------------------------------------------------------------------
---- Instances
--------------------------------------------------------------------------

--instance Monoid VAState where
--    mempty      = VAState mempty mempty
--    mappend a b = VAState (mappend (a ^. aa)      (b ^. aa))
--                          (mappend (a ^. idStack) (b ^. idStack))
