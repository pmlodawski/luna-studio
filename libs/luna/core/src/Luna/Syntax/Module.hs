---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE UndecidableInstances #-}

module Luna.Syntax.Module where

import GHC.Generics      (Generic)
import Flowbox.Prelude

import Luna.Syntax.Name  (TName)
import Luna.Syntax.Decl  (LDecl)
import Luna.Syntax.Label (Label)
import Luna.Syntax.Name.Path (QualPath)


data Module a e = Module { _mpath :: QualPath
                         , _body  :: [LDecl a e]
                         } deriving (Show, Eq, Generic, Read)


type LModule a e = Label a (Module a e)