---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Luna.Data.HAST.DataType (
        module Flowbox.Luna.Data.HAST.DataType,
        module Flowbox.Luna.Data.HAST.Expr
)where

import Flowbox.Luna.Data.HAST.Expr

empty :: Expr
empty = DataD "" [] [] []

