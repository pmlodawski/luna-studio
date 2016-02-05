---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Luna.Target.HS.AST.Comment where

import           Flowbox.Prelude

data Comment = H1 { text :: Text }
             | H2 { text :: Text }
             | H3 { text :: Text }
             | H4 { text :: Text }
             | H5 { text :: Text }
             deriving (Show)


