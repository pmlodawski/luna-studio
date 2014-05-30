---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.AWS.EC2.Instance.Instance where

import qualified Data.IP   as IP
import           Data.Text (Text)

import qualified Flowbox.Data.Time as Time
import           Flowbox.Prelude



type ID = Text

data Instance = Instance { key     :: Int
                         , id      :: ID
                         , ip_addr :: IP.IPv4
                         --, started ::
                         } deriving (Show, Ord, Eq, Read)
