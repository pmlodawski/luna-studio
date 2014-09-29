---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}

module Luna.Data.Source where

import Flowbox.Prelude



data Source = Source { _path :: [String]
                     , _code :: String
                     } deriving (Show)

makeLenses(''Source)



transCode :: (String -> String) -> Source -> Source
transCode = (code %~ )
