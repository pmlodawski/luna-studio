
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TupleSections #-}


module Main where

import Flowbox.Prelude
import Data.Repr

import qualified Luna.Inference.Type as Type
import           Luna.Inference.Type (Type)
import qualified Data.Map            as Map
import           Data.Map            (Map)
import qualified Data.Text.Lazy      as Text
import           Data.Text.Lazy      (Text)

import qualified Luna.Inference.Object as Object
import           Luna.Inference.Object (Object)
import           Luna.Inference.Data (Data, pack, unpack)
import qualified Luna.Inference.Data as Data


add :: Int -> Int -> Int
add = (+)

add2 :: (Int, (Int, ())) -> Int
add2 (a, (b, ())) = a + b

type DataFunc = [Data] -> Data

testf :: (Int, (String, ())) -> (Int,String)
testf (a,(b,())) = (a,b)

unpackTest :: Object -> (Int,String)
unpackTest = Object.unpack

toDataFunc :: (ToRArgs args, Data.Pack out) => (args -> out) -> DataFunc
toDataFunc f = pack . f . toRArgs

class ToRArgs args where
    toRArgs :: [Data] -> args

instance ToRArgs () where
    toRArgs [] = ()

instance ToRArgs as => ToRArgs (a, as) where
    toRArgs (a:as) = (Data.unpack a, toRArgs as)


--class ToRFunc f args out | f -> args out where
--    toRFunc :: f -> (args -> out)

--instance ToDataFunc (a,(b,())) c where
--    toDataFunc f = \[a,b] -> pack $ add (unpack a) (unpack b)

add_Int :: DataFunc
add_Int [a, b] = pack $ add (unpack a) (unpack b)

appSimple :: DataFunc -> [Object] -> Object
appSimple f objs = Object.simple . f $ fmap Object.unpack objs

unpackInt :: Object -> Int
unpackInt = Object.unpack

main = do
    let a = Object.simple (1 :: Int)
        b = Object.simple (2 :: Int)
        s = Object.simple ("ala" :: String)
        --c = appSimple add_Int [a,b]
        c = appSimple (toDataFunc add2) [a,b]
        x = appSimple (toDataFunc testf) [a,s]

    print $ unpackInt a
    print $ unpackInt b
    print $ unpackInt c
    print $ unpackTest x
    print "end"


