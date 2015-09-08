---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

{-# LANGUAGE FunctionalDependencies #-}

module Luna.Target.HS.Data.Func.Func where

import Luna.Target.HS.Data.Struct

----------------------------------------------------------------------------------
-- Type classes
----------------------------------------------------------------------------------

--class Func (base :: k) name args out | base name args -> out where
--    getFunc :: Mem base name -> args -> (args -> out)


class FuncProvider base name func where
    getFunc2 :: Mem base name -> (func -> out) -> func

