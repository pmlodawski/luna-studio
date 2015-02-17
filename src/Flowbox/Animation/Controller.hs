---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.Animation.Controller where

import           Control.Monad
import qualified Data.Map as Map

import           Flowbox.Animation.Model 
import           Flowbox.Prelude



valueAt :: (Ord x) => NonNumericModel x y -> x -> Maybe y
valueAt (NonNumericModel segs) x = join . fmap snd $ Map.lookupLE x segs

insertValue :: (Ord x) => NonNumericModel x y -> x -> y -> NonNumericModel x y
insertValue (NonNumericModel segs) x y = NonNumericModel $ Map.insert x (Just y) segs

insertEmpty :: (Ord x) => NonNumericModel x y -> x -> NonNumericModel x y
insertEmpty (NonNumericModel segs) x = NonNumericModel $ Map.insert x Nothing segs

create :: (Ord x) => x -> y -> NonNumericModel x y 
create x y = NonNumericModel $ Map.singleton x $ Just y

