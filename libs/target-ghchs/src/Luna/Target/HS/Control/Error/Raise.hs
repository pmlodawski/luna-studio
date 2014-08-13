---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}

{-# LANGUAGE OverlappingInstances #-}
--{-# LANGUAGE IncoherentInstances #-}

{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE CPP #-}


!{-# LANGUAGE RightSideContexts #-}
!{-# LANGUAGE Python #-}

module Luna.Target.HS.Control.Error.Raise where


import Luna.Target.HS.Control.Error.Data


------------------------------------------------------------------------
-- Type classes
------------------------------------------------------------------------

class Raise e a b | e a -> b where
    raise :: e -> a -> b 


class TouchErr e a b | e a -> b where
    touchErr :: e -> a -> b 


------------------------------------------------------------------------
-- Instances
------------------------------------------------------------------------

instance Raise e (Safe a)               (UnsafeBase Safe e a)     where raise e (Safe a) = Error e
instance Raise e (UnsafeBase base e a)  (UnsafeBase base e a)     where raise e a = Error e
instance Raise e (UnsafeBase base be a) (UnsafeBase outBase be a) <= (Raise e (base a) (outBase a)) where
    raise e base = case base of
        UnsafeValue val   -> UnsafeValue val
        Error       err   -> Error err
        UnsafeOther base' -> UnsafeOther $ raise e base'


instance TouchErr e (Safe a)               (UnsafeBase Safe e a)     where touchErr e (Safe a) = UnsafeValue a
instance TouchErr e (UnsafeBase base e a)  (UnsafeBase base e a)     where touchErr e a = a
instance TouchErr e (UnsafeBase base be a) (UnsafeBase outBase be a) <= (TouchErr e (base a) (outBase a)) where
    touchErr e base = case base of
        UnsafeValue val   -> UnsafeValue val
        Error       err   -> Error err
        UnsafeOther base' -> UnsafeOther $ touchErr e base'


