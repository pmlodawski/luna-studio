---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Flowbox.Luna.Passes.Transform.AST.TxtParser.ParseState where

import           Flowbox.Prelude                  hiding(id)
import           Flowbox.Luna.Data.AST.SourcePos  (SourceRange)
import           Flowbox.Luna.Data.Pass.SourceMap (SourceMap)
import qualified Flowbox.Luna.Data.Pass.SourceMap as SourceMap
import           Flowbox.Luna.Data.AST.Utils      (ID)
import qualified Flowbox.Luna.Data.Pass.ASTInfo                    as ASTInfo
import           Flowbox.Luna.Data.Pass.ASTInfo                    (ASTInfo)

data ParseState = ParseState { _info       :: ASTInfo
                             , _sourceMap  :: SourceMap
                             , _lastLexeme :: String
                             }
                deriving (Show)
                

makeLenses ''ParseState


registerSrc :: ID -> SourceRange -> ParseState -> ParseState
registerSrc id src state = state & sourceMap %~ (SourceMap.insert id src)

------------------------------------------------------------------------
-- Instances
------------------------------------------------------------------------

instance Default ParseState where
		def = ParseState def def ""


mk :: ASTInfo -> ParseState
mk i = def & info .~ i