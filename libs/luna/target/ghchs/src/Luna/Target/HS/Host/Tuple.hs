---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DysfunctionalDependencies #-}

module Luna.Target.HS.Host.Tuple where

import Data.Tuple.Select
import Control.PolyMonad
import Data.RTuple (RTuple(RTuple))
import Data.Wrapper
import qualified Data.RTuple as RT
import Luna.Target.HS.Control.Flow.Env (val, Value, Pure, Safe)

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



class ExtractRTuple m r r' | r -> r' where 
    extractRTuple :: m r -> r'

instance (Functor m1, PolyMonad m1 m2 m3, ExtractRTuple m1 as t)
      => ExtractRTuple m1 (m2 a,as) (m3 a, t) where
    extractRTuple r = (polyJoin $ fmap RT.head r, extractRTuple $ fmap RT.tail r)

instance ExtractRTuple m1 () () where
    extractRTuple _ = ()

instance (Functor m, ExtractRTuple m a b) => ExtractRTuple m (RTuple a) (RTuple b) where
    extractRTuple = wrap . extractRTuple . fmap unwrap

---

class MapRTVal t t' | t -> t' where
    mapRTVal :: t -> t'

instance MapRTVal () () where
    mapRTVal = id

instance MapRTVal as bs => MapRTVal (a,as) (Value Pure Safe a,bs) where
    mapRTVal (a,as) = (val a, mapRTVal as)

instance MapRTVal a b => MapRTVal (RTuple a) (RTuple b) where
    mapRTVal (RTuple a) = RTuple $ mapRTVal a

---

--class MapRTFlattenEnv t t' | t -> t' where
--    mapRTFlattenEnv :: t -> t'

--instance MapRTFlattenEnv () () where
--    mapRTFlattenEnv = id

--instance (FlattenEnv a b, MapRTFlattenEnv as bs) => MapRTFlattenEnv (a,as) (b,bs) where
--    mapRTFlattenEnv (a,as) = (flattenEnv a, mapRTFlattenEnv as)

--instance MapRTFlattenEnv a b => MapRTFlattenEnv (RTuple a) (RTuple b) where
--    mapRTFlattenEnv (RTuple a) = RTuple $ mapRTFlattenEnv a