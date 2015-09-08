---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Luna.DEP.Data.HAST.DataType (
        module Luna.DEP.Data.HAST.DataType,
        module Luna.DEP.Data.HAST.Expr
)where

import Luna.DEP.Data.HAST.Expr

empty :: Expr
empty = DataD "" [] [] []

