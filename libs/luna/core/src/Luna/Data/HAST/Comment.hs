---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Luna.Data.HAST.Comment where

import Flowbox.Prelude

data Comment = H1 { text :: String }
             | H2 { text :: String }
             | H3 { text :: String }
             | H4 { text :: String }
             | H5 { text :: String }
             deriving (Show)


