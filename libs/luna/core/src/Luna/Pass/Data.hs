---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Luna.Pass.Data where

import           Flowbox.Prelude
import qualified Data.HTSet.HTSet as HTSet
import           Data.HTSet.HTSet (HTSet)


----------------------------------------------------------------------
-- Data types
----------------------------------------------------------------------

data DataInfo = DataInfo { _name :: String
                         , _desc :: String
                         } deriving (Show)

data PassData a = PassData { _info :: DataInfo
                           } 

makeLenses ''DataInfo
makeLenses ''PassData




