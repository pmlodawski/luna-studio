{-# LANGUAGE FunctionalDependencies, FlexibleInstances #-}

module FlowboxM.Luna.FClasses.U_z where

class FC_z a b | a -> b where
    _z :: a -> b

class Arg_z m a b | m -> a, m a -> b where
    setArg_z :: m a -> b


