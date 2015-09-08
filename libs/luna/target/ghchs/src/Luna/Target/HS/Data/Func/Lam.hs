---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------


{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}

module Luna.Target.HS.Data.Func.Lam where

import Luna.Target.HS.Data.Func.App
import Data.Typeable
import Luna.Target.HS.Control.Flow.Env
import Luna.Target.HS.Data.Func.Args9 
import qualified Luna.Target.HS.Data.Func.Args9 as Args
import Data.PolyTypeable (PolyTypeable, polyTypeOf)

----------------------------------------------------------------------------------
-- Data types
----------------------------------------------------------------------------------

data Lam lam = Lam lam deriving (Typeable)

instance (l~Func n args f, PolyTypeable f) => Show (Lam l) where
	show (Lam (Func (_,f))) = "Lam " ++ (show $ polyTypeOf f)

----------------------------------------------------------------------------------
-- Utils
----------------------------------------------------------------------------------

--mkLam lam args = val $ AppH (Lam lam, args)

mkLam :: sig -> f -> Value Pure Safe (AppH (Lam (Func (ArgsKind sig) sig f)) ())
mkLam sig f = val $ AppH (Lam $ Args.func sig f, ())