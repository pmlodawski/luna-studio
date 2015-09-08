---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Luna.Syntax.Unit where

import Flowbox.Prelude
import GHC.Generics    (Generic)
import Luna.Syntax.Module (LModule)
import qualified Luna.Syntax.Module as Module
import Luna.Syntax.Decl   (Imp)

data Unit a = Unit { _fromUnit :: a } deriving (Generic, Show, Functor)

type ASTUnit a e = Unit (LModule a e)

makeLenses ''Unit


-- == Utils ==

imports :: ASTUnit a e -> [Imp]
imports = Module.imports . unwrap . unwrap

-- == Instances ==

instance Unwrap  Unit where unwrap = view fromUnit
instance Wrap    Unit where wrap   = Unit
instance Wrapper Unit
