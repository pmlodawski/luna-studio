{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverlappingInstances #-}

{-# LANGUAGE DysfunctionalDependencies #-}

!{-# LANGUAGE RightSideContexts #-}

import GHC.TypeLits

import Luna.Target.HS.Proxy

----------------------------------------------------------------------------------
-- Data types
----------------------------------------------------------------------------------

newtype Arg (name :: Symbol) a = Arg a deriving Show
data    NoArg (name :: Symbol) = NoArg deriving Show
newtype DefaultArg (name :: Symbol) a = DefaultArg a deriving Show

newtype FPtr ptr = FPtr ptr deriving Show

unnamedArg = NoArg :: NoArg ""


----------------------------------------------------------------------------------
-- Prop
----------------------------------------------------------------------------------

class Prop (name :: Symbol) obj fptr | name obj -> fptr where
    prop :: Proxy name -> obj -> fptr




newtype AppH fptr args = AppH (fptr, args) deriving Show
appH fptr args = AppH (fptr, args)

class AppProto el fptr fptrout | el fptr -> fptrout where
    app :: el -> fptr -> fptrout


----------------------------------------------------------------------------------
-- AppNextArg
----------------------------------------------------------------------------------

class AppNextArg val args out | val args -> out where
    appNextArg :: val -> args -> out

instance AppNextArg val (NoArg name, args) (Arg name val, args') <= (args~args') where
    appNextArg val (_, args) = (Arg val, args)

instance AppNextArg val (DefaultArg name dval, args) (Arg name val, args') <= (args~args', val~dval) where
    appNextArg val (_, args) = (Arg val, args)

instance AppNextArg val (a, args) (a', out) <= (a~a', AppNextArg val args out) where
    appNextArg val (a, args) = (a, appNextArg val args)

----------------------------------------------------------------------------------
-- AppArgByName
----------------------------------------------------------------------------------

class AppArgByName (name :: Symbol) val args out | name val args -> out where
    appArgByName :: Proxy name -> val -> args -> out


instance AppArgByName name val (NoArg name, args) (Arg name val, args') <= (args~args') where
    appArgByName _ val (_, args) = (Arg val, args)

instance AppArgByName name val (Arg name oldval, args) (Arg name val, args') <= (args~args') where
    appArgByName _ val (_, args) = (Arg val, args)


instance AppArgByName name val (a, args) (a', out) <= (a~a', AppArgByName name val args out) where
    appArgByName name val (a, args) = (a, appArgByName name val args)

----------------------------------------------------------------------------------
-- ReadArgs
----------------------------------------------------------------------------------

class ReadArgs args vals | args -> vals where
    readArgs :: args -> vals

instance ReadArgs (Arg name a, args) (a, out) <= ReadArgs args out where
    readArgs (Arg a, args) = (a, readArgs args)

instance ReadArgs (DefaultArg name a, args) (a, out) <= ReadArgs args out where
    readArgs (DefaultArg a, args) = (a, readArgs args)

instance ReadArgs () () where
    readArgs = id


----------------------------------------------------------------------------------
-- Tests
----------------------------------------------------------------------------------

data Vector a = Vector { x :: a
                       , y :: a
                       , z :: a
                       } deriving Show

newtype Prop_Vector_length obj = Prop_Vector_length obj deriving Show

instance Prop "length" (Vector a) (AppH (Prop_Vector_length (Vector a)) (NoArg "",(NoArg "",(NoArg "",())))) where
    prop _ obj = appH (Prop_Vector_length obj) (unnamedArg,(unnamedArg,(unnamedArg,()))) 


main = do
    let v = Vector (1::Int) (2::Int) (3::Int)
        p = prop (Proxy::Proxy "length") v 
        args = (unnamedArg,(NoArg :: NoArg "dupa",(unnamedArg,())))
        args2 = (unnamedArg,(NoArg :: NoArg "dupa",(DefaultArg 0 :: DefaultArg "def" Int,())))
    print $ readArgs $ appNextArg (1::Int) $ appNextArg (1::Int) $ appNextArg (1::Int) $ args
    print $ appArgByName (Proxy :: Proxy "dupa") (2::Int) $ appArgByName (Proxy :: Proxy "dupa") (1::Int) $ args

    putStrLn "---"

    print $ readArgs $ appNextArg (1::Int) $ appNextArg (1::Int) $ args2
    --print $ app (1::Int) p
    --print $ app (3::Int) $ app (2::Int) $ app (1::Int) p
--    --print $ call $ app (3::Int) $ app (2::Int) $ app (1::Int) p
    print "end"


