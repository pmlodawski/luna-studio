{-# LANGUAGE FunctionalDependencies, FlexibleInstances #-}

module FlowboxM.Luna.FClasses.U_main where

class FC_main a b | a -> b where
    _main :: a -> b

class Arg_main m a b | m -> a, m a -> b where
    setArg_main :: m a -> b


