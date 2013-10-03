{-# LANGUAGE FunctionalDependencies, FlexibleInstances #-}

module FlowboxM.Luna.FClasses.U_a where

class FC_a a b | a -> b where
    _a :: a -> b

class Arg_a m a b | m -> a, m a -> b where
    setArg_a :: m a -> b


