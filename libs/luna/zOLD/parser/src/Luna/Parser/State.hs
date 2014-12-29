---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

{-# LANGUAGE TemplateHaskell #-}

module Luna.Parser.State where

import           Flowbox.Prelude     hiding (id)
import           Luna.AST.Common     (ID)
import           Luna.Data.ASTInfo   (ASTInfo)
import           Luna.Data.SourceMap (SourceMap)
import qualified Luna.Data.SourceMap as SourceMap
import           Luna.Data.SourcePos (SourceRange)

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
