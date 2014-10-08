---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleInstances #-}

module Luna.AST.IDMap (
    module Luna.AST.IDMap,
    module X
) where

import qualified Data.IntMap as IntMap
import           Data.IntMap as X hiding (IntMap)


type IDMap = IntMap.IntMap