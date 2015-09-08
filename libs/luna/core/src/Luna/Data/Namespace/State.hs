---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}

module Luna.Data.Namespace.State where

import           Control.Monad.State (MonadState)
import qualified Data.IntMap         as IntMap

import qualified Flowbox.Data.MapForest    as MapForest
import           Flowbox.Prelude           hiding (id)
import           Flowbox.System.Log.Logger as L
import           Luna.Data.Namespace       (Namespace, NamespaceMonad)
import qualified Luna.Data.Namespace       as Namespace
import           Luna.Data.StructInfo      (OriginInfo, StructInfo, StructInfoMonad)
import qualified Luna.Data.StructInfo      as StructInfo
import           Luna.Syntax.AST           (AST, ID)
import qualified Luna.Syntax.AST           as AST
import           Luna.Syntax.Decl          (Path)
import           Luna.Syntax.Expr          (Expr)
import qualified Luna.Syntax.Expr          as Expr
import           Luna.Syntax.Lit           (Lit)
import qualified Luna.Syntax.Lit           as Lit
import           Luna.Syntax.Module        (Module)
import qualified Luna.Syntax.Module        as Module
import           Luna.Syntax.Name.Path     (NamePath)
import qualified Luna.Syntax.Name.Path     as NamePath
import           Luna.Syntax.Name.Pattern  (NamePatDesc)
import           Luna.Syntax.Pat           (Pat)
import qualified Luna.Syntax.Pat           as Pat
import           Luna.Syntax.Type          (Type)
import qualified Luna.Syntax.Type          as Type


logger :: LoggerIO
logger = getLoggerIO $moduleName


--data VAState = VAState { _info    :: StructInfo
--                       , _idStack :: [ID]
--                       }
--             deriving (Show)

--makeLenses (''VAState)

type NamespaceState m = (Monad m, Applicative m, NamespaceMonad m, StructInfoMonad m)   -- ++ StructDataMonad m ?


--getCurrentID :: NamespaceState m => m (Maybe ID)
--getCurrentID = do stack <- view idStack <$> get
--                  return $ case stack of
--                      []    -> Nothing
--                      (x:_) -> Just x

modify f = do
    ns <- Namespace.get
    Namespace.put $ f ns

scopeID = Namespace.head <$> Namespace.get


--putStructInfo :: NamespaceState m => StructInfo -> m ()
putStructInfo info = modify (Namespace.info .~ info)


--modifyStructInfo :: NamespaceState m => (StructInfo -> StructInfo) -> m ()
modifyStructInfo f = do
    info <- StructInfo.get
    putStructInfo $ f info



--pushID :: NamespaceState m => ID -> m ()
pushID id = modify $ Namespace.pushID id

--popID :: NamespaceState m => m ID
popID = do
    (id, ns') <- Namespace.popID <$> Namespace.get
    Namespace.put ns'
    return id



--pushID :: NamespaceState m => ID -> m ()
--pushID id = modify (idStack %~ (id:))

--popID :: NamespaceState m => m ID
--popID = do (id:ids) <- view idStack <$> get
--           modify (idStack .~ ids)
--           return id


--withID :: NamespaceState m => ID -> m f -> m f
--withID id f = pushID id *> f <* popID

-- creates the base scope, in which you store the imported symbols

pushNewScope id = modify $ Namespace.pushNewScope id
pushScope    id = modify $ Namespace.pushScope id

popScope = modify Namespace.popScope

withNewScope id p = pushNewScope id *> p <* popScope
withScope    id p = pushScope    id *> p <* popScope


withParentID :: NamespaceState m => m f -> m f
withParentID f = do pid <- popID
                    out <- f
                    pushID pid
                    return out

----switchID :: NamespaceState m => ID -> m ()
----switchID id = modify (currentID .~ id)
regOrphan = modifyStructInfo .: StructInfo.regOrphan


--regID :: NamespaceState m => ID -> m ()
regID id = do
    mpid <- scopeID
    withJust mpid (\pid -> modifyStructInfo $ StructInfo.parent %~ IntMap.insert id pid)


regVarName :: NamespaceState m => OriginInfo -> NamePath -> m ()
regVarName = regName StructInfo.varnames



regNamePatDesc :: NamespaceState m => ID -> NamePatDesc -> m ()
regNamePatDesc id argPat = modifyStructInfo (StructInfo.argPats %~ IntMap.insert id argPat)

regParent :: NamespaceState m => ID -> m ()
regParent id = do
    scopeID <- scopeID
    withJust scopeID (\pid -> modifyStructInfo (StructInfo.parent %~ IntMap.insert id pid))


regAlias :: NamespaceState m => ID -> NamePath -> m ()
regAlias ident name = do
    structInfo <- StructInfo.get
    -- TODO [kgdk]: remove Just
    structInfo' <- withCurrentScopeID_ (\scopeId -> Just $ StructInfo.regAlias ident name scopeId structInfo)
    modify $ Namespace.info .~ structInfo'


-- regOrphan :: NamespaceState m => ID -> Path -> String -> m ()
-- regOrphan id path msg = do
--     structInfo <- StructInfo.get
--     structInfo' <- withCurrentScopeID_ (\scopeId -> Just $ StructInfo.regOrphan id (StructInfo.ImportError path (toText msg)))
--     modify $ Namespace.info .~ structInfo'


withCurrentScopeID_ :: NamespaceState m => (ID -> Maybe a) -> m a
withCurrentScopeID_ action = withCurrentScopeID action >>= maybe (fail "Cannot obtain current scope") return

withCurrentScopeID :: NamespaceState m => (ID -> Maybe a) -> m (Maybe a)
withCurrentScopeID action = scopeID >>= (return . maybe Nothing action)

withCurrentScope_ :: NamespaceState m => (StructInfo.Scope -> Maybe a) -> m a
withCurrentScope_ action = withCurrentScope action >>= maybe (fail "Cannot obtain current scope") return


withCurrentScope :: NamespaceState m => (StructInfo.Scope -> Maybe a) -> m (Maybe a)
withCurrentScope action = getCurrentScope >>= (return . maybe Nothing action)


getCurrentScope :: NamespaceState m => m (Maybe StructInfo.Scope)
getCurrentScope = scopeID >>= \case
    Just (currentScopeId :: ID) -> view (StructInfo.scope . at currentScopeId) <$> StructInfo.get
    Nothing                     -> return Nothing


regTypeName :: NamespaceState m => OriginInfo -> NamePath -> m ()
regTypeName = regName StructInfo.typenames


regName lens id name = do
    a    <- StructInfo.get
    mcid <- scopeID
    case mcid of
        Nothing  -> fail "Unable to get current id"
        Just cid -> putStructInfo a2
            where varRel  = a ^. (StructInfo.scope . ix cid)
                  varRel2 = varRel & lens %~ MapForest.insert (NamePath.toList name) id
                  a2      = a & StructInfo.scope.at cid ?~ varRel2


test x y = do
    a    <- StructInfo.get
    mcid <- scopeID
    return ()

--regParentVarName :: NamespaceState m => ID -> String -> m ()
--regParentVarName = withParentID .: regVarName

--bindVar id name = do
--    ns <- get
--    case Namespace.bindVar id name ns of
--        Left _    -> fail $ "Unable to bind variable " ++ name -- FIXME[wd]: nicer error messages
--        Right ns' -> put ns'

--tryBindVar id name = do
--    ns <- get
--    case Namespace.bindVar id name ns of
--        Left _    -> do let errMsg = "Unable to bind variable " ++ name -- FIXME[wd]: nicer error messages
--                        logger L.error errMsg
--                        regOrphan id name
--        Right ns' -> put ns'


--bindVar :: NamespaceState m => ID -> String -> m ()
--bindVar id name = do
--    mcid <- getCurrentID
--    withJust mcid (\cid -> modifyStructInfo (bindVarRec id cid name))


--bindVarRec :: ID -> ID -> String -> StructInfo -> StructInfo
--bindVarRec id ctxID name a = case dstIDLookup of
--    Just dstID -> updateAliasMap dstID
--    Nothing    -> case mPid of
--                  Just pid -> bindVarRec id pid name a
--                  Nothing  -> updateInvalidMap $ StructInfo.LookupError name
--    where dstIDLookup          = varnames ^. at name
--          mPid                 = (a ^. StructInfo.parent) ^. at ctxID
--          varRel               = a ^. StructInfo.scope.ix ctxID
--          varnames             = varRel ^. StructInfo.varnames
--          updateAliasMap val   = a & StructInfo.alias.at id ?~ val
--          updateInvalidMap val = a & StructInfo.orphans.at id ?~ val



--------------------------------------------------------------------------
---- Instances
--------------------------------------------------------------------------

--instance Monoid VAState where
--    mempty      = VAState mempty mempty
--    mappend a b = VAState (mappend (a ^. aa)      (b ^. aa))
--                          (mappend (a ^. idStack) (b ^. idStack))


