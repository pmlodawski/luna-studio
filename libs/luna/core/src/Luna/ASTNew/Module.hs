---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE UndecidableInstances #-}

module Luna.ASTNew.Module where

import GHC.Generics     (Generic)
import Flowbox.Prelude
import Luna.ASTNew.Name   (TName)
import Luna.ASTNew.Decl   (RDecl, RCons, Decl, Field)
import Luna.ASTNew.Pat    (RPat)
import Luna.ASTNew.Type   (RType)
import Luna.ASTNew.Native (Native)


data Module f e = Module { _path :: [TName]
                         , _name :: TName
                         , _body :: [RDecl f e]
                         } deriving (Generic)

----------------------------------------------------------------------
-- Instances
----------------------------------------------------------------------

deriving instance (Show (f (Decl f e)), Show (f (Field f e)), Show (RPat f), Show (RType f), Show (Native (RDecl f e)), Show (RCons f e), Show e) => Show (Module f e)