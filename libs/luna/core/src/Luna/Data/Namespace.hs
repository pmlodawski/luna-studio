---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

{-# LANGUAGE NoMonomorphismRestriction #-}

module Luna.Data.Namespace where


import GHC.Generics        (Generic)

import           Flowbox.Prelude     hiding (head)
import qualified Data.Maps           as Map
import           Data.Map            (Map)
import           Luna.AST.Name       (Name)
import           Luna.AST.AST        (ID)
import           Luna.Data.AliasInfo (AliasInfo)
import qualified Luna.Data.AliasInfo as Alias
import           Data.Maybe          (fromJust)
import qualified Luna.AST.AST        as AST

----------------------------------------------------------------------
-- Data types
----------------------------------------------------------------------

data Namespace = Namespace { _stack :: [ID]
                           , _info :: AliasInfo
                           } deriving (Show, Eq, Generic, Read)


makeLenses ''Namespace

----------------------------------------------------------------------
-- Utils
----------------------------------------------------------------------

--lookup :: String -> Namespace -> Maybe Name
--lookup name ns = ns^.scope.nameMap.at name


head :: Namespace -> Maybe ID
head (Namespace (id:_) _) = Just id
head _                    = Nothing


pushScope :: ID -> Namespace -> Namespace
pushScope id ns@(Namespace st inf) = ns
                                   & pushID id
                                   & info .~ ninfo
    where ninfo  = inf & Alias.scope .~ scope
          scopes = view Alias.scope inf
          pScope = case head ns of
              Nothing  -> def
              Just pid -> fromJust $ Map.lookup pid scopes
          scope  = Map.insert id pScope scopes

popScope :: Namespace -> Namespace
popScope = snd . popID

pushID :: ID -> Namespace -> Namespace
pushID id = stack %~ (id:)

popID :: Namespace -> (ID, Namespace)
popID ns = (id, ns & stack .~ ids)
    where (id:ids) = view stack ns

bindVar :: ID -> String -> Namespace -> Either () Namespace
bindVar id name ns = 
    case head ns of
        Nothing  -> Left ()
        Just pid -> case view (info.Alias.scope.at pid) ns of
            Nothing    -> Left ()
            Just (Alias.Scope varnames typenames) -> case (varnames^.at name) of 
                Nothing    -> Left ()
                Just dstID -> Right (ns & info . Alias.alias . at id ?~ dstID)


regAST id ast = info %~ Alias.regAST id (AST.wrap ast)



regParent id pid = info %~ Alias.regParent id pid



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

modAlias :: (AliasInfo -> AliasInfo) -> Namespace -> Namespace
modAlias f = info %~ f

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
                            