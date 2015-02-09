---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Luna.Data.HAST.DataType (
        module Luna.Data.HAST.DataType,
        module Luna.Data.HAST.Expr
)where

import Luna.Data.HAST.Expr

empty :: Expr
empty = DataD "" [] [] []

