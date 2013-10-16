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
import           Data.Tuple.OneTuple   
import           Data.Typeable         


import           Debug.Trace           

instance (Typeable a) => Show (IO a) where
    show e = '(' : (show . typeOf) e ++ ")"

instance (Typeable a, Typeable b) => Show (a -> b) where
    show e = '(' : (show . typeOf) e ++ ")"


newtype Pure a    = Pure    { getPure :: a } deriving(Typeable)
--newtype FuncRef a = FuncRef { getFunc :: a } deriving(Typeable)

instance Show a => Show (Pure a) where
    show (Pure a) = "Pure " ++ show a

newtype Func a = Func a deriving(Show, Typeable)


class Bind m1 m2 where
    bind :: m1 a -> (Pure a -> m2 b) -> IO b


instance (GetIO a, GetIO b) => Bind a b where
    bind a b = do
        va <- getIO a
        getIO $ b (Pure va)


bind_ a b = bind a (\_ -> b)

-----------------

class GetIO f where 
    getIO :: f a -> IO a

instance GetIO Pure where
    getIO (Pure a) = return a

--instance GetIO FuncRef where
--    getIO a = return a

instance GetIO IO where
    getIO = id



-----------------


type List a = [a]