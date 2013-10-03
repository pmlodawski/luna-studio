{-# LANGUAGE FunctionalDependencies, FlexibleInstances #-}

module FlowboxM.Luna.FClasses.U_add where

class FC_add a b | a -> b where
    _add :: a -> b

class Arg_add m a b | m -> a, m a -> b where
    setArg_add :: m a -> b


