---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

{-# LANGUAGE TemplateHaskell #-}

module Luna.DEP.Parser.Operator where

import           Data.Map                (Map)
import qualified Data.Map                as Map
import           Flowbox.Prelude
import           Luna.DEP.Data.ASTInfo   (ASTInfo)
import           Luna.DEP.Data.Config    (Config)
import qualified Luna.DEP.Data.Config    as Config
import           Luna.DEP.Data.SourceMap (SourceMap)
import qualified Luna.DEP.Data.SourceMap as SourceMap


------------------------------------------------------------------------
-- Data Types
------------------------------------------------------------------------

type Precedence  = Int
type Name        = String
type OperatorMap = Map Name Operator

data Fixity = Prefix
            | Infix
            | PostFix
            deriving (Show)


data Associativity = None
                   | Left
                   | Right
                   deriving (Show)


data Operator = Operator { _precedence    :: Precedence
                         , _fixity        :: Fixity
                         , _associativity :: Associativity
                         } deriving (Show)

makeLenses ''Operator

