---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}



import           Flowbox.Prelude                        

import           Control.Monad.State                    
import           Control.Monad.Writer                   
import           Control.Monad.RWS                      
import           Control.Monad.Trans.Maybe              
import           Control.Monad.Trans.Either             
import           Flowbox.System.Log.Logger              
import qualified Flowbox.System.Log.Logger            as Logger
import qualified Flowbox.System.Log.LogEntry          as LogEntry
import qualified Flowbox.Luna.Passes.HSGen.HSGen      as HSGen
import qualified Flowbox.Luna.Passes.HSGen.HSC        as HSC
import qualified Flowbox.Luna.Passes.SSA.SSA          as SSA
import qualified Flowbox.Luna.Passes.HSGen.AST.Module as Module
import qualified Flowbox.Luna.Passes.HSGen.AST.Expr   as Expr
import qualified Flowbox.Luna.Passes.SSA.State        as SSAState
import           Flowbox.Luna.Passes.SSA.State          (SSAState)
import qualified Flowbox.Luna.Passes.Luna.Luna        as Luna
import qualified Flowbox.Luna.Passes.Txt2AST.Txt2AST  as Txt2AST
import qualified Flowbox.Luna.Data.Source             as Source
import           Flowbox.Luna.Data.Source               (Source)
import qualified Flowbox.Text.Show.Pretty             as PP

import           Debug.Trace                            
import           Data.Either.Utils                      (forceEither)
import qualified Data.DList                           as DList

import           Control.Applicative                    

import           System.TimeIt                          



logger :: Logger
logger = getLogger "Flowbox"


example :: Source
example = Source.Source "Workspace"
        $ unlines [ "import Std.Math.Scalar"
                  , ""
                  , "class Vector a:"
                  , "    x :: a"
                  , ""
                  , "def f(x::Int, y::Map Int Float):"
                  , "    z = x + 7"
                  --, "    g = (x, y)"
                  , "    z+ y"
                  , ""
                  , "def g a b:"
                  , "    c = 1"
                  , "    a = b + 1"
                  , "    _ = a + b"
                  ]


main :: IO ()
main = do
    out <- timeIt main_inner
    case out of
        Right _ -> return ()
        Left  e -> putStrLn e

main_inner :: IO (Either String ())
main_inner = Luna.run $ do
    putStrLn "\n-------- AST --------"
    ast <- Txt2AST.run example
    --putStrLn $ PP.ppShow ast
    putStrLn $ PP.ppqShow ast

    --putStrLn "\n-------- SSA --------"
    --ssa <- SSA.run     ast
    --putStrLn $ PP.ppShow ssa

    --putStrLn "\n-------- HAST --------" 
    --hast <- HSGen.run  ssa
    --putStrLn $ PP.ppShow hast

    --putStrLn "\n-------- HSC --------" 
    --hsc <- HSC.run  hast
    --putStrLn $ hsc

    return ()



--a :: (Int) -> (Int, Int)


--g = @f 5

--def f (a,b): a+b

--def f (a): a+b

--(x): x+1



--a = 1

--Vector x y z = v

--(a :: Vector 0 0 0) = v

-- ~[a,b=1] = [1]


--def f x=0 y=0 z=0:
--    x+y+z

--f 1 {z=1}

--[0..100].each x:
--    print x

--Std.Math.Vector x y z = v
--v = Std.Math.Vector 0 0 0


--def f (Vector x y z :: Vector Int)


--def add a b : a+b

--def add (a,b) : a+b

--[1..100].each \x:
    

--a :: Int, Int -> Int
--a :: Int -> String, Int
--a :: Int -> String, (Int, Int -> Int)

--def f a ::(Int -> String, (Int, Int -> Int)) b :: (Int) :

--def f (a :: Int -> String, (Int, Int -> Int), b :: Int) :

--def f (a::Int->(String, Int), b::Int->Int):
--    ...


----a :: (Int,Int) -> Int

----a :: (Int,Int) -> ((Int,Int))

----a :: (Int,Int -> Int,Int)
----a :: ((Int,Int) -> Int)

----a :: Int -> Int -> Int


--def subdivide (self, divs=2, preserveNormals=True):
--    ...

--g.subdivide divs=4 

--def subdivide (self, divs::Int, preserveNormals::Bool)

--def subdivide (self, divf::((Geometry, Int)) -> Geometry, )

--def subdivide (self, div::{Geometry, Int} -> Geometry)


--a = [10..20]
--b = [20..10]
--(a.zip b).each {x,y}:



--def f


--{x,y,z} = {1,2,3}

--f a b c=3

--f(a,b,c=3)


--def f x y z :
--    x + y + z


--[1..100].each {x, y}:
--    print x

--def add (Vector a, b, c):
--    a+b

--[1..100].each x:
--    print x

--f = \x y:4 :





--a :: (Int, Int) -> Int

--def a (x,y): 
--    x+y


--def f a::Int b::Int :     # FAIL

--def f a::Int, b::Int :    # OK

--def f (a::Int) (b::Int) : # OK

--def f (a::Int, b::Int) :  # OK




--a :: Int -> Int -> {Int, Int}

--a :: (Int, Int) -> {Int, Int}


--def f (a::Int->Int, b):


--def f (a,b):


--def f {a,b}:

--g = {a,b}: a+b

--g {1,2}


--(x::Int, y::Int):


-- \x y -> x+y

-- \(x::Int) (y::Int) -> x+y

--{x::Int, y::Int}: x+y

--{1,2,3}

--{1:1, 2:2, 3:3}


--f 1 2

--f (1,2) 3

--f ((1,2), 3)

----------- 1 ---------
-- # types:
--a :: Int -> (Int, Int)

--b :: (Int, Int) -> (String, String)

-- # definitions od 2 arguments functions:

--def f x y: x+y

--def f (x::Int) y:
--    x + y

--def f (x::Int) (y::String):
--    x + y

-- # definition of 1 argument (a tuple) function:

--def g (x,y): x+y

--def g (x::Int, y::String):
--    x + y

-- # usage:
--f 1 2
--g (1,2)

----------- 2 ---------
-- # types:
--a :: Int -> (Int, Int)

--b :: (Int, Int) -> (String, String)

-- # definitions od 2 arguments functions:

--def f (x, y): x+y

--def f (x::Int, y):
--    x + y

--def f (x::Int, y::String):
--    x + y

-- # definition of 1 argument (a tuple) function:

--def g ((x,y)): x+y

--def g (x::Int, y::String):
--    x + y

-- # usage:
--f 1 2
--g (1,2)












