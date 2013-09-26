{-# LANGUAGE FunctionalDependencies, FlexibleInstances #-}

module FlowboxM.Luna.FClasses.U_y where

class FC_y a b | a -> b where
    _y :: a -> b

class Arg_y m a b | m -> a, m a -> b where
    setArg_y :: m a -> b


