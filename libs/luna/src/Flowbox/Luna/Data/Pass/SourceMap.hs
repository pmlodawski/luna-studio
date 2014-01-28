---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Flowbox.Luna.Data.Pass.SourceMap (
    module Flowbox.Luna.Data.Pass.SourceMap,
    module Data.Map
)where

import Data.Map

import Flowbox.Luna.Data.AST.SourcePos (SourceRange)
import Flowbox.Luna.Data.AST.Utils     (ID)


type SourceMap = Map ID SourceRange

