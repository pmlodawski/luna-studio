---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Flowbox.Luna.Passes.Transform.AST.TxtParser.Token where

import Flowbox.Prelude
import Flowbox.Luna.Data.AST.SourcePos (SourceRange)

data Token = Token { text  :: String
                   , range :: SourceRange
                   }
           deriving (Show)