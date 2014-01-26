---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE ViewPatterns           #-}

--{-# LANGUAGE NoMonomorphismRestriction #-}


module Luna.Target.HS.Naming where

import           Control.Applicative
import           Control.Monad
import           Debug.Trace
import           Language.Haskell.TH
import           Language.Haskell.TH.Lib
import           Luna.Target.HS.TH.Utils
import qualified Text.Show.Pretty        as PP


showName = mkName "Show"
liftName = mkName "val"
liftE    = VarE liftName
liftP    = VarP liftName

--withName :: (String -> String) -> (Name -> Name)
--withName f = mkName.f.nameBase

class NameCls a where
    withName :: (String -> String) -> (a -> a)
    toString :: a -> String
    toName   :: a -> Name

instance NameCls String where
    withName f = f
    toString   = id
    toName     = mkName

instance NameCls Name where
    withName f = mkName.f.nameBase
    toString   = nameBase
    toName     = id

mkHandlerName :: NameCls a => a -> a
mkHandlerName = withName ("Handler_" ++)

mkHandlerFuncName :: NameCls a => a -> a
mkHandlerFuncName = withName ("h_" ++)

mkConName :: NameCls a => a -> a
mkConName = withName ("con_" ++)

mkCallName :: NameCls a => a -> a
mkCallName = withName ("call_" ++)

mkLamName :: NameCls a => a -> a
mkLamName = withName ("l_" ++)

mkSetName :: NameCls a => a -> a
mkSetName = withName ("set_" ++)

mkMemName :: (NameCls a, NameCls b) => a -> b -> b
mkMemName dataName memberName = withName (("m_" ++ toString dataName ++ "_") ++) memberName

mkMemGetName :: (NameCls a, NameCls b) => a -> b -> b
mkMemGetName dataName memberName = withName (("mget_" ++ toString dataName ++ "_") ++) memberName

mkMemSetName :: (NameCls a, NameCls b) => a -> b -> b
mkMemSetName dataName memberName = withName (("mset_" ++ toString dataName ++ "_") ++) memberName

mkPropertyName :: (NameCls a, NameCls b) => a -> b -> b
mkPropertyName dataName memberName = withName (("p_" ++ toString dataName ++ "_") ++) memberName

showD        = mkName "Show"

mVar       = mkName "m"
mVarT      = VarT mVar

sVar       = mkName "s"
sVarT      = VarT sVar


memberClass     = mkName "MemberProto"
memberClassFunc = mkName "memberProto"


baseFieldName :: Name -> Name -> Name
baseFieldName (nameBase -> clsName) (nameBase -> hashName) = mkName $ drop (prefixlen) hashName where
    prefixlen = 3 + length clsName
