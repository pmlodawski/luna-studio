{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}

module Luna.Parser.State where

import           Flowbox.Prelude
import           Luna.Data.ASTInfo    (ASTInfo)
import           Luna.Data.SourceMap  (SourceMap)
import qualified Luna.Data.SourceMap  as SourceMap
import qualified Luna.Data.Config     as Config
import           Luna.Data.Config     (Config)
import qualified Data.Map             as Map
import           Data.Map             (Map)
import           Luna.Parser.Operator (OperatorMap)



data State a = State { _conf       :: Config a
                     , _info       :: ASTInfo
                     , _opFixity   :: OperatorMap
                     , _sourceMap  :: SourceMap
                     , _lastLexeme :: String
                     } deriving (Show)

makeLenses ''State


------------------------------------------------------------------------
-- Utils
------------------------------------------------------------------------

mk :: ASTInfo -> State ()
mk i = def & info .~ i


------------------------------------------------------------------------
-- Instances
------------------------------------------------------------------------

instance a~() => Default (State a) where
        def = State def def def def ""



