---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric #-}

module Luna.ASTNew.Module where

import GHC.Generics     (Generic)
import Flowbox.Prelude
import Luna.ASTNew.Name (Name, VName, TName, CName, TVName)
import Luna.ASTNew.Expr (Expr)


data Module f = Module { _path :: [TName]
                       , _name :: TName
                       , _body :: [Expr f]
                       , _mods :: [Module f] 
                       } deriving (Generic)