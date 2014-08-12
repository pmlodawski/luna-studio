---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}

!{-# LANGUAGE RightSideContexts #-}

module Luna.Target.HS.Data.Prop where

import GHC.TypeLits  (Symbol)
import Data.Typeable (Proxy)


----------------------------------------------------------------------------------
-- Prop
----------------------------------------------------------------------------------

class Prop (name :: Symbol) obj fptr | name obj -> fptr where
    prop :: Proxy name -> obj -> fptr
