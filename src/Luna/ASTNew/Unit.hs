---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Luna.ASTNew.Unit where

import GHC.Generics      (Generic)
import Flowbox.Prelude

data Unit a = Unit a deriving (Generic, Show)