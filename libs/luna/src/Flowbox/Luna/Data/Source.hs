---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Luna.Data.Source where

import           Flowbox.Prelude   



data Source = Source { path :: [String]
                     , code :: String
                     } deriving (Show)


transCode :: (String -> String) -> Source -> Source
transCode f src = src { code = f $ code src }