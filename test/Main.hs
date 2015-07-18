
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Main where

import Flowbox.Prelude hiding (simple)
import Data.Repr

--import qualified Luna.Inference.Type as Type
import           Luna.Inference.Type
import qualified Data.Map            as Map
import           Data.Map            (Map)
import qualified Data.Text.Lazy      as Text
import           Data.Text.Lazy      (Text)

import           Luna.Inference.RawData

import           Luna.Inference.Function

import           GHC.Prim (Any)
import           Unsafe.Coerce (unsafeCoerce)
import           Data.Convert

add :: Int -> Int -> Int
add = (+)

add2 :: (Int, (Int, ())) -> Int
add2 (a, (b, ())) = a + b


testf :: (Int, (String, ())) -> (Int,String)
testf (a,(b,())) = (a,b)

--unpackTest :: Object -> (Int,String)
--unpackTest = unpackRawData


--unpackInt :: Object -> Int
--unpackInt = unpackRawData

f = unsafeCoerce add :: Any

appx :: a -> Any -> Any
appx a f = (unsafeCoerce f :: Any -> Any) (unsafeCoerce a :: Any)

main = do
    let
        --a = mkObject (1 :: Int)
        --b = mkObject (2 :: Int)
        --s = mkObject ("ala" :: String)
        --c = appSimple add_Int [a,b]
        --c = appSimple (toDataFunc add2) [a,b]
        --x = appSimple (toDataFunc testf) [a,s]


    --print $ unpackInt a
    --print $ unpackInt b
    --print $ unpackInt c
    --print $ unpackTest x

    print $ (unsafeCoerce $ appx (1 :: Int) $ appx (2 :: Int) f :: Int)
    print "end"


