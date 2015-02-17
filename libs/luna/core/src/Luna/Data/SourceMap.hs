---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Luna.Data.SourceMap (
    module Luna.Data.SourceMap,
    module Data.Map
)where

import Data.Map
import Flowbox.Prelude

import Luna.Data.SourcePos (SourceRange)

type ID = Int

type SourceMap = Map ID SourceRange

