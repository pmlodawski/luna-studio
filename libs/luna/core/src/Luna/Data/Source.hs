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

makeLenses ''Source



transCode :: (String -> String) -> Source -> Source
transCode = (code %~ )



--{-# LANGUAGE TemplateHaskell #-}

--module Luna.Data.Source where

--import Flowbox.Prelude
--import qualified Data.ByteString.UTF8 as UTF8



--data Source = File       { _path  :: String          }
--            | String     { _input :: String          }
--            | ByteString { _input :: UTF8.ByteString }
--            deriving (Show)

--makeLenses(''Source)


----transCode :: (String -> String) -> Source -> Source
----transCode = (code %~ )
