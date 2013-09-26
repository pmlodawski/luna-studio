{-# LANGUAGE FunctionalDependencies, FlexibleInstances #-}

module FlowboxM.Luna.FClasses.U_x where

class FC_x a b | a -> b where
    _x :: a -> b

class Arg_x m a b | m -> a, m a -> b where
    setArg_x :: m a -> b


