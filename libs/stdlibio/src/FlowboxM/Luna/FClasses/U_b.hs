{-# LANGUAGE FunctionalDependencies, FlexibleInstances #-}

module FlowboxM.Luna.FClasses.U_b where

class FC_b a b | a -> b where
    _b :: a -> b

class Arg_b m a b | m -> a, m a -> b where
    setArg_b :: m a -> b


