---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Flowbox Team <contact@flowbox.io>, 2014
-- Proprietary and confidential
-- Unauthorized copying of this file, via any medium is strictly prohibited
---------------------------------------------------------------------------
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module Flowbox.Utils where

import Data.Typeable (Typeable, typeOf)
import Prelude


------------------------------------------------------------------------
-- Display utils
------------------------------------------------------------------------

instance (Typeable a) => Show (IO a) where
    show e = '<' : (show . typeOf) e ++ ">"

instance (Typeable a, Typeable b) => Show (a -> b) where
    show e = '<' : (show . typeOf) e ++ ">"

------------------------------------------------------------------------
-- Func utils
------------------------------------------------------------------------

printTyped :: (Typeable a, Show a) => a -> IO ()
printTyped x = putStrLn $ show x ++ " :: " ++ show (typeOf x)

printType :: (Typeable a) => a -> IO ()
printType x = putStrLn $ "_ :: " ++ show (typeOf x)


dot0  = ($)
dot1  = (.)
dot2  = dot1 . (.)
dot3  = dot2 . (.)
dot4  = dot3 . (.)
dot5  = dot4 . (.)
dot6  = dot5 . (.)
dot7  = dot6 . (.)
dot8  = dot7 . (.)
dot9  = dot8 . (.)
dot10 = dot9 . (.)



curryFirst :: (t2 -> t1) -> (t1 -> t) -> t2 -> t
curryFirst f g = (\a -> g (f a))

fcurry1 :: t1 -> (t1 -> x) -> x
fcurry1 = flip ($)

fcurry2 :: t1 -> t2 -> (t1 -> t2 -> x) -> x
fcurry2 t = (flip dot1) (fcurry1 t) `dot1` fcurry1

fcurry3 :: t1 -> t2 -> t3 -> (t1 -> t2 -> t3 -> x) -> x
fcurry3 t = (flip dot1) (fcurry1 t) `dot2` fcurry2

fcurry4 :: t1 -> t2 -> t3 -> t4 -> (t1 -> t2 -> t3 -> t4 -> x) -> x
fcurry4 t = (flip dot1) (fcurry1 t) `dot3` fcurry3

fcurry5 :: t1 -> t2 -> t3 -> t4 -> t5 -> (t1 -> t2 -> t3 -> t4 -> t5 -> x) -> x
fcurry5 t = (flip dot1) (fcurry1 t) `dot4` fcurry4

fcurry6 :: t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> x) -> x
fcurry6 t = (flip dot1) (fcurry1 t) `dot5` fcurry5


--appLastArg1 f = (curryFirst f) `dot1` fcurry1
--appLastArg2 f = (curryFirst f) `dot2` fcurry2
--appLastArg3 f = (curryFirst f) `dot3` fcurry3
--appLastArg4 f = (curryFirst f) `dot4` fcurry4
--appLastArg5 f = (curryFirst f) `dot5` fcurry5


appArg0 = id
appArg1 = ($)
appArg2 f x = flip appArg1 x . f
appArg3 f x = flip appArg2 x . f
appArg4 f x = flip appArg3 x . f
appArg5 f x = flip appArg4 x . f
appArg6 f x = flip appArg5 x . f
appArg7 f x = flip appArg6 x . f
appArg8 f x = flip appArg7 x . f
appArg9 f x = flip appArg8 x . f

--appArg2 f x t1 = f t1 x
