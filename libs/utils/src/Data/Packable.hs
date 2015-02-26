---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2015
---------------------------------------------------------------------------

{-# LANGUAGE FunctionalDependencies #-}

module Data.Packable where


----------------------------------------------------------------------------------
-- Type classes
----------------------------------------------------------------------------------

class Unpack cntr b | cntr -> b where
    unpack :: cntr -> b

class Pack a cntr | cntr -> a where
    pack :: a -> cntr


class (Unpack a b, Pack b a) => Packable a b
