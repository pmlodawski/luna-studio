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
import Luna.ASTNew.Name (TName)
import Luna.ASTNew.Decl (Decl)


data Module f e = Module { _path :: [TName]
                         , _name :: TName
                         , _body :: [Decl f e]
                         , _mods :: [Module f e] 
                         } deriving (Generic)