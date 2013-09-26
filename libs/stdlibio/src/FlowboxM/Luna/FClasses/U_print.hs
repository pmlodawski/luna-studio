{-# LANGUAGE FunctionalDependencies, FlexibleInstances #-}

module FlowboxM.Luna.FClasses.U_print where

class FC_print a b | a -> b where
    _print :: a -> b

class Arg_print m a b | m -> a, m a -> b where
    setArg_print :: m a -> b
