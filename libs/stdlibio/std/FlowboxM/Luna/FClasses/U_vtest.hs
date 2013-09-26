{-# LANGUAGE FunctionalDependencies, FlexibleInstances #-}

module FlowboxM.Luna.FClasses.U_vtest where

class FC_vtest a b | a -> b where
    _vtest :: a -> b

class FC_vtest2 a b | a -> b where
    _vtest2 :: a -> b

