---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell           #-}

module Luna.Data.Namespace where


import GHC.Generics (Generic)

import qualified Data.Maps           as Map
import           Data.Maybe          (fromJust)
import           Flowbox.Prelude     hiding (head, id)
import           Luna.AST.AST        (ID)
import           Luna.Data.AliasInfo (AliasInfo)
import qualified Luna.Data.AliasInfo as Alias


----------------------------------------------------------------------
-- Data types
----------------------------------------------------------------------

data Namespace = Namespace { _stack :: [ID]
                           , _alias :: AliasInfo
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
pushScope id ns@(Namespace st a) = Namespace (id:st) (set Alias.scope scope a) where
    scopes = view Alias.scope a
    pScope = case head ns of
        Nothing  -> def
        Just pid -> fromJust $ Map.lookup pid scopes
    scope  = Map.insert id pScope scopes

popScope :: Namespace -> Namespace
popScope (Namespace (_:st) a) = Namespace st a

------------------------------------------------------------------------
---- Instances
------------------------------------------------------------------------

--instance Default Scope where
--    def = Scope def

instance Default Namespace where
    def = Namespace def def
