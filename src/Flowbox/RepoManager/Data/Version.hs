---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.RepoManager.Data.Version (
    module Data.Version,
    Strictness(..),
    Constraint(..),
    parseVersion
) where

import 			 Flowbox.Prelude
import 			 Data.Version                 hiding (parseVersion)
import qualified Data.Version                 as Version (parseVersion)
import qualified Text.ParserCombinators.ReadP as ReadP

data Strictness = Strict | NotStrict
                deriving (Show)

data Constraint = Minimum Strictness Version
                | Exactly            Version
                | Maximum Strictness Version
                | Any
                deriving (Show)

parseVersion :: String -> Version
parseVersion = fst . last . ReadP.readP_to_S Version.parseVersion