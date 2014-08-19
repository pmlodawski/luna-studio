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
{-# LANGUAGE ScopedTypeVariables #-}

--{-# LANGUAGE DysfunctionalDependencies #-}

!{-# LANGUAGE RightSideContexts #-}

import GHC.TypeLits

import Luna.Target.HS.Proxy
import Flowbox.Utils
import Utils

import Luna.Target.HS.Func.Args as Args hiding (main)
--import qualified Luna.Target.Func.HS.Args as Args 

----------------------------------------------------------------------------------
-- Data types
----------------------------------------------------------------------------------

newtype AppH fptr args = AppH (fptr, args) deriving Show
appH fptr args = AppH (fptr, args)

----------------------------------------------------------------------------------
-- Prop
----------------------------------------------------------------------------------

class Prop (name :: Symbol) obj fptr | name obj -> fptr where
    prop :: Proxy name -> obj -> fptr


----------------------------------------------------------------------------------
-- Func
----------------------------------------------------------------------------------

class Func fptr args out | fptr args -> out where
    getFunc :: AppH fptr args -> out

call apph@(AppH(fptr, args)) = getFunc apph args


----------------------------------------------------------------------------------
-- Instances
----------------------------------------------------------------------------------

instance AppNextArg val (AppH fptr args) (AppH fptr out) <= (AppNextArg val args out) where
    appNextArg val (AppH (fptr, args)) = AppH (fptr, appNextArg val args)

instance AppArgByName name val (AppH fptr args) (AppH fptr out) <= (AppArgByName name val args out) where
    appArgByName name val (AppH (fptr, args)) = AppH (fptr, appArgByName name val args)


appByName = matchCall `dot3` appArgByName
appNext   = matchCall `dot2` appNextArg


----------------------------------------------------------------------------------
-- MatchCall
----------------------------------------------------------------------------------


class MatchCallProto (allArgs :: Bool) obj out | allArgs obj -> out where
    matchCallProto :: Proxy allArgs -> obj -> out

instance MatchCallProto False a a where
    matchCallProto _ = id

instance MatchCallProto True (AppH fptr args) out <= (ReadArgs args margs, Func fptr margs (margs -> out)) where
    matchCallProto _ (AppH (fptr, args)) = call $ AppH (fptr, readArgs $ args)


class MatchCall obj out | obj -> out where
    matchCall :: obj -> out

instance MatchCall (AppH fptr args) out <= (MatchCallProto flag (AppH fptr args) out, AllArgs args flag) where
    matchCall = matchCallProto (undefined :: Proxy flag)


----------------------------------------------------------------------------------
-- Tests
----------------------------------------------------------------------------------

foo (a,(b,())) = (a,b)

data Vector a = Vector { x :: a
                       , y :: a
                       , z :: a
                       } deriving Show

newtype Prop_Vector_length obj = Prop_Vector_length obj deriving Show

instance Prop "length" (Vector a) (AppH (Prop_Vector_length (Vector a)) (Unnamed (Untyped Unprovided),(Unnamed (Untyped Unprovided),()))) where
    prop _ obj = appH (Prop_Vector_length obj) (mkArg::Param,(mkArg::Param,()))

instance Func (Prop_Vector_length (Vector a)) (a,(b,())) ((a,(b,())) -> (a,b)) where
    getFunc _ = foo


main = do
    let v = Vector (1::Int) (2::Int) (3::Int)
        p = prop (Proxy::Proxy "length") v 
        args = (mkArg::Param,(Named (Untyped Unprovided) :: Named "dupa" (Untyped Unprovided),(mkArg::Param,())))
        args2 = (mkArg::Param,(Named (Typed Unprovided) :: Named "dupa" (Typed Int Unprovided),(Named (Untyped (Default 0)) :: Named "def" (Untyped (Default Int)),())))
        --args3 = (unnamedArg,(Typed Unprovided :: Typed Int (Unprovided "dupa"),(Default 0 :: Default "def" Int,())))
    print $ appNextArg (1::Int) $ appNextArg (1::Int) $ appNextArg (1::Int) $ args
    print $ readArgs $ appNextArg (1::Int) $ appNextArg (1::Int) $ appNextArg (1::Int) $ args
            --            --printType $ allArgs $ appNextArg (1::Int) $ appNextArg (1::Int) $ appNextArg (1::Int) $ args
    print $ appArgByName (Proxy :: Proxy "dupa") (2::Int) $ appArgByName (Proxy :: Proxy "dupa") (1::Int) $ args

    putStrLn "---"

    print $ appNextArg (1::Int) $ appNextArg (1::Double) $ args2

    print $ appNext (1::Int) $ appNext (1::Int) $ p


    print "end"



--main = Args.main
