
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TupleSections #-}


module Main where

import Flowbox.Prelude hiding (simple)
import Data.Repr

--import qualified Luna.Inference.Type as Type
import           Luna.Inference.Type
import qualified Data.Map            as Map
import           Data.Map            (Map)
import qualified Data.Text.Lazy      as Text
import           Data.Text.Lazy      (Text)

import           Luna.Inference.Value

import qualified Luna.Inference.Function as F


add :: Int -> Int -> Int
add = (+)

add2 :: (Int, (Int, ())) -> Int
add2 (a, (b, ())) = a + b

type DataFunc = [Value] -> Value

testf :: (Int, (String, ())) -> (Int,String)
testf (a,(b,())) = (a,b)

unpackTest :: Object -> (Int,String)
unpackTest = unpackRawData

toDataFunc :: ToRArgs args => (args -> out) -> DataFunc
toDataFunc f = packRawData . f . toRArgs

class ToRArgs args where
    toRArgs :: [Value] -> args

instance ToRArgs () where
    toRArgs [] = ()

instance ToRArgs as => ToRArgs (a, as) where
    toRArgs (a:as) = (unpackRawData a, toRArgs as)





appSimple :: DataFunc -> [Object] -> Object
appSimple f objs = mkObject . f $ fmap unpackRawData objs

unpackInt :: Object -> Int
unpackInt = unpackRawData

main = do
    let a = mkObject (1 :: Int)
        b = mkObject (2 :: Int)
        s = mkObject ("ala" :: String)
        --c = appSimple add_Int [a,b]
        c = appSimple (toDataFunc add2) [a,b]
        x = appSimple (toDataFunc testf) [a,s]

    print $ unpackInt a
    print $ unpackInt b
    print $ unpackInt c
    print $ unpackTest x
    print "end"


