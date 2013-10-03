{-# LANGUAGE FunctionalDependencies, FlexibleInstances #-}

module FlowboxM.Luna.FClasses.U_length where

class FC_length a b | a -> b where
    _length :: a -> b

class Arg_length m a b | m -> a, m a -> b where
    setArg_length :: m a -> b


