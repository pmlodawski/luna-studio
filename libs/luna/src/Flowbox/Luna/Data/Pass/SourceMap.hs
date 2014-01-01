---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Flowbox.Luna.Data.Pass.SourceMap where

import qualified Data.Map as Map
import           Data.Map (Map)

import Flowbox.Prelude                 hiding(id)
import Flowbox.Luna.Data.AST.SourcePos (SourcePos)
import Flowbox.Luna.Data.AST.Utils     (ID)


type SourceMap = Map ID SourcePos
