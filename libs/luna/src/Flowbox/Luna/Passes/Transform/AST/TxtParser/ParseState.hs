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

data ParseState = ParseState { _id        :: Int
                             , _sourceMap :: SourceMap
                             }
                deriving (Show)
                

makeLenses ''ParseState


registerSrc :: ID -> SourceRange -> ParseState -> ParseState
registerSrc id src state = state & sourceMap %~ (SourceMap.insert id src)

------------------------------------------------------------------------
-- Instances
------------------------------------------------------------------------

instance Default ParseState where
	def = ParseState 0 def


make :: ID -> ParseState
make i = ParseState i SourceMap.empty