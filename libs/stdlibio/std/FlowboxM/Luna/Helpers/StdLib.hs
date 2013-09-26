{-# LANGUAGE MultiParamTypeClasses, 
             FunctionalDependencies, 
             NoMonomorphismRestriction,
             DeriveDataTypeable,
             ScopedTypeVariables,
             FlexibleInstances, 
             UndecidableInstances,
             ExtendedDefaultRules,

             GADTs#-}

module FlowboxM.Luna.Helpers.StdLib where

import           Control.Applicative
import           System.Random         (randomIO)
import           Data.Tuple.OneTuple
import           Data.Typeable


import Debug.Trace


instance (Typeable a) => Show (IO a) where
    show e = '(' : (show . typeOf) e ++ ")"

instance (Typeable a, Typeable b) => Show (a -> b) where
    show e = '(' : (show . typeOf) e ++ ")"


newtype Pure a = Pure a deriving(Typeable)

instance Show a => Show (Pure a) where
    show (Pure a) = "Pure " ++ show a
    --show (Pure a) = show a

newtype Func a = Func a deriving(Show, Typeable)

--newtype Funcref a = Funcref a deriving(Show, Typeable)



