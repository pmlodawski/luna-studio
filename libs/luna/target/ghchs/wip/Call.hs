{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
--{-# LANGUAGE OverlappingInstances #-}

{-# LANGUAGE DysfunctionalDependencies #-}

!{-# LANGUAGE RightSideContexts #-}

import GHC.TypeLits

import Luna.Target.HS.Proxy


data Arg a = Arg a deriving Show
data NoArg = NoArg deriving Show


class Prop (name :: Symbol) obj fptr | name obj -> fptr where
    prop :: Proxy name -> obj -> fptr


data Vector a = Vector { x :: a
                       , y :: a
                       , z :: a
                       } deriving Show


--newtype FPtr ptr args = FPtr (ptr, args) deriving Show
newtype FPtr ptr = FPtr ptr deriving Show

--fptr ptr args = FPtr (ptr, args)

newtype Prop_Vector_length obj = Prop_Vector_length obj deriving Show

instance Prop "length" (Vector a) ((Arg(Arg(Arg(FPtr (Prop_Vector_length (Vector a)))))) )where
    prop _ obj = (Arg(Arg(Arg(FPtr (Prop_Vector_length obj))))) 


newtype AppH fptr args = AppH (fptr, args) deriving Show
appH fptr args = AppH (fptr, args)

class AppProto el fptr fptrout | el fptr -> fptrout where
    app :: el -> fptr -> fptrout


instance AppProto el (Arg a) out <= AppProto el (AppH (Arg a) ()) out where
    app el fptr = app el $ appH fptr ()

instance AppProto el (AppH (Arg (Arg fptr)) args) (AppH (Arg fptr) (el, args)) where
    app el (AppH (Arg fptr, args)) = appH fptr (el, args)

instance AppProto el (AppH (Arg (FPtr fptr)) args) out <= Call fptr (el, args) out where
    app el (AppH (Arg fptr, args)) = call $ appH fptr (el, args)

class Call fptr args out | fptr args -> out where
    call :: AppH (FPtr fptr) args -> out


instance Call (Prop_Vector_length (Vector a)) (a,(b,(c,()))) Int where
    call _ = 5

main = do
    let v = Vector (1::Int) (2::Int) (3::Int)
        p = prop (Proxy::Proxy "length") v 
    print $ app (3::Int) $ app (2::Int) $ app (1::Int) p
    --print $ call $ app (3::Int) $ app (2::Int) $ app (1::Int) p
    print "end"


