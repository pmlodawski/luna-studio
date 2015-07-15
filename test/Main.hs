
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

import           Luna.Inference.Data (Data, Value, toValue, fromValue)
import qualified Luna.Inference.Data as Data


add :: Int -> Int -> Int
add = (+)

add2 :: (Int, (Int, ())) -> Int
add2 (a, (b, ())) = a + b

type DataFunc = [Data] -> Data

testf :: (Int, (String, ())) -> (Int,String)
testf (a,(b,())) = (a,b)

unpackTest :: Object -> (Int,String)
unpackTest = unpack

toDataFunc :: ToRArgs args => (args -> out) -> DataFunc
toDataFunc f = fromValue . f . toRArgs

class ToRArgs args where
    toRArgs :: [Data] -> args

instance ToRArgs () where
    toRArgs [] = ()

instance ToRArgs as => ToRArgs (a, as) where
    toRArgs (a:as) = (Data.unpack a, toRArgs as)





appSimple :: DataFunc -> [Object] -> Object
appSimple f objs = mkObject . f $ fmap unpack objs

unpackInt :: Object -> Int
unpackInt = unpack

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


