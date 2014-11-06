---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

{-# LANGUAGE NoMonomorphismRestriction #-}

module Luna.Target.HS.Host.Tuple where

import Data.Tuple.Select
import Control.PolyMonad

tupGet1 = polyJoin . fmap sel1
tupGet2 = polyJoin . fmap sel2
tupGet3 = polyJoin . fmap sel3
tupGet4 = polyJoin . fmap sel4
tupGet5 = polyJoin . fmap sel5
tupGet6 = polyJoin . fmap sel6
tupGet7 = polyJoin . fmap sel7
tupGet8 = polyJoin . fmap sel8
tupGet9 = polyJoin . fmap sel9

extractTuple0 x = ()
extractTuple1 x = x
extractTuple2 x = (tupGet1 x, tupGet2 x)
extractTuple3 x = (tupGet1 x, tupGet2 x, tupGet3 x)
extractTuple4 x = (tupGet1 x, tupGet2 x, tupGet3 x, tupGet4 x)
extractTuple5 x = (tupGet1 x, tupGet2 x, tupGet3 x, tupGet4 x, tupGet5 x)
extractTuple6 x = (tupGet1 x, tupGet2 x, tupGet3 x, tupGet4 x, tupGet5 x, tupGet6 x)
extractTuple7 x = (tupGet1 x, tupGet2 x, tupGet3 x, tupGet4 x, tupGet5 x, tupGet6 x, tupGet7 x)
extractTuple8 x = (tupGet1 x, tupGet2 x, tupGet3 x, tupGet4 x, tupGet5 x, tupGet6 x, tupGet7 x, tupGet8 x)
extractTuple9 x = (tupGet1 x, tupGet2 x, tupGet3 x, tupGet4 x, tupGet5 x, tupGet6 x, tupGet7 x, tupGet8 x, tupGet9 x)
