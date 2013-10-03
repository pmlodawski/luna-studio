
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.System.Random where

import           Control.Applicative   
import qualified Data.Time.LocalTime as LocalTime
import qualified Data.Hash.MD5       as MD5

import           Flowbox.Prelude     hiding (error)



newGUID :: IO String
newGUID = do 
    t <- LocalTime.getZonedTime
    return $ MD5.md5s $ MD5.Str $ show t