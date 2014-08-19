---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PackageImports #-}

module Control.Monad.Morph (
    module Control.Monad.Morph,
    module X
) where

import "mmorph" Control.Monad.Morph as X

class MonadMorph m n where
    morph :: m a -> n a
