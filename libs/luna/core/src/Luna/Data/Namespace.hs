---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverlappingInstances      #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE UndecidableInstances      #-}

module Luna.Data.Namespace where


import           GHC.Generics              (Generic)

import           Control.Monad.RWS         (RWST)
import qualified Control.Monad.RWS         as RWST
import           Control.Monad.Trans.Class (MonadTrans, lift)
import qualified Data.IntMap               as IntMap
import qualified Data.Maps                 as Map
import           Data.Maybe                (fromJust)
import           Data.Maybe                (fromJust)
import           Flowbox.Prelude           hiding (head, id)
import           Luna.Data.StructInfo      (StructInfo, StructInfoMonad)
import qualified Luna.Data.StructInfo      as StructInfo
import qualified Luna.Syntax.Name.Path     as NamePath

import qualified Flowbox.Data.MapForest    as MapForest


----------------------------------------------------------------------
-- Data types
----------------------------------------------------------------------
type ID = Int

data Namespace = Namespace { _stack :: [ID]
                           , _info  :: StructInfo
                           } deriving (Show, Eq, Generic, Read)


makeLenses ''Namespace

----------------------------------------------------------------------
-- Type classes
----------------------------------------------------------------------

class NamespaceMonad m where
    get :: m Namespace
    put :: Namespace -> m ()

----------------------------------------------------------------------
-- Utils
----------------------------------------------------------------------

--lookup :: String -> Namespace -> Maybe Name
--lookup name ns = ns^.scope.nameMap.at name


head :: Namespace -> Maybe ID
head (Namespace (id:_) _) = Just id
head _                    = Nothing


-- FIXME[wd]: dodac asserty!
pushNewScope :: ID -> Namespace -> Namespace
pushNewScope id ns@(Namespace st inf) = ns
                                   & pushID id
                                   & info .~ ninfo
    where ninfo  = inf & StructInfo.scope .~ scope
          scopes = view StructInfo.scope inf
          pScope = case head ns of
              Nothing  -> def
              Just pid -> fromJust $ Map.lookup pid scopes
          scope  = Map.insert id pScope scopes

-- FIXME[wd]: dodac asserty!
pushScope :: ID -> Namespace -> Namespace
pushScope id ns = case view (info . StructInfo.scope . at id) ns of
    Just _  -> pushExistingScope id ns
    Nothing -> pushNewScope id ns

pushExistingScope :: ID -> Namespace -> Namespace
pushExistingScope = pushID

popScope :: Namespace -> Namespace
popScope = snd . popID

pushID :: ID -> Namespace -> Namespace
pushID id = stack %~ (id:)

popID :: Namespace -> (ID, Namespace)
popID ns = (id, ns & stack .~ ids)
    where (id:ids) = view stack ns

--bindVar :: ID -> String -> Namespace -> Either () (Namespace)
--bindVar id name ns =
--    case head ns of
--        Nothing  -> Left ()
--        Just pid -> case view (info.StructInfo.scope.at pid) ns of
--            Nothing    -> Left ()
--            Just (StructInfo.Scope varnames typenames) -> case (varnames^.at name) of
--                Nothing    -> Left ()
--                Just dstID -> Right (ns & info . StructInfo.StructInfo . at id ?~ dstID)


regParent id pid = info %~ StructInfo.regParent id pid

regOrphan id err = info %~ StructInfo.regOrphan id err

regOrigin id origin = modStructInfo (StructInfo.regOrigin id origin)

--pushID :: ID -> m ()
--pushID id = modify (idStack %~ (id:))

--popID :: VAMonad m => m ID
--popID = do (id:ids) <- view idStack <$> get
--           modify (idStack .~ ids)
--           return id



--pushScopeM id = do
--    s <- get
--    put $ pushScope id s

----popScopeM     = popScope id <$> get

--withScope id p = do
--    pushScope id
--    ret <- p
--    popScope
--    return ret

modStructInfo :: (StructInfo -> StructInfo) -> Namespace -> Namespace
modStructInfo f = info %~ f

------------------------------------------------------------------------
---- Instances
------------------------------------------------------------------------

--instance Default Scope where
--    def = Scope def

instance Default Namespace where
    def = Namespace def def

instance Monoid Namespace where
    mempty      = Namespace mempty mempty
    mappend a b = Namespace (mappend (a ^. stack) (b ^. stack))
                            (mappend (a ^. info)  (b ^. info))


instance (Monad m, Monoid w) => NamespaceMonad (RWST r w Namespace m) where
    get = RWST.get
    put = RWST.put

-- default instances

instance (MonadTrans t, NamespaceMonad m, Monad m) => NamespaceMonad (t m) where
    get = lift get
    put = lift . put

instance (Monad m, NamespaceMonad m) => StructInfoMonad m where
    get = do
        ns <- get
        return $ ns ^. info
    put i = do
        ns <- get
        put (ns & info .~ i)


