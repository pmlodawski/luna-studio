---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.Test.Hspec.Lifted (
    module Flowbox.Test.Hspec.Lifted,
    module X
) where

import           Flowbox.Prelude
import           Test.Hspec      as X



shouldBe' :: (Show a, Eq a, MonadIO m) => a -> a -> m ()
shouldBe' = liftIO .: shouldBe


shouldMatchList' :: (Show a, Eq a, MonadIO m) => [a] -> [a] -> m ()
shouldMatchList' = liftIO .: shouldMatchList


shouldSatisfy' :: (Show a, MonadIO m) => a -> (a -> Bool) -> m ()
shouldSatisfy' = liftIO .: shouldSatisfy
