{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Number.Conversion where

import           Data.Word
import           GHC.Float
import           Prelude   hiding (toInteger)
import qualified Prelude


class Convertible a b where
    convert :: a -> b


instance (Num a) => Convertible Int a where
    convert = fromIntegral

instance (Num a) => Convertible Integer a where
    convert = fromIntegral

instance (Num a) => Convertible Word8 a where
    convert = fromIntegral

instance (Num a) => Convertible Word16 a where
    convert = fromIntegral

instance (Num a) => Convertible Word32 a where
    convert = fromIntegral

instance (Num a) => Convertible Word64 a where
    convert = fromIntegral

------

instance Convertible Float Int where
    convert = round

instance Convertible Float Integer where
    convert = round

instance Convertible Float Float where
    convert = id

instance Convertible Float Double where
    convert = float2Double

instance Convertible Float Word8 where
    convert = round

instance Convertible Float Word16 where
    convert = round

instance Convertible Float Word32 where
    convert = round

instance Convertible Float Word64 where
    convert = round

------

instance Convertible Double Int where
    convert = round

instance Convertible Double Integer where
    convert = round

instance Convertible Double Float where
    convert = double2Float

instance Convertible Double Double where
    convert = id

instance Convertible Double Word8 where
    convert = round

instance Convertible Double Word16 where
    convert = round

instance Convertible Double Word32 where
    convert = round

instance Convertible Double Word64 where
    convert = round



toInt :: Convertible a Int => a -> Int
toInt = convert

toInteger :: Convertible a Integer => a -> Integer
toInteger = convert

toFloat :: Convertible a Float => a -> Float
toFloat = convert

toDouble :: Convertible a Double => a -> Double
toDouble = convert

main = do
    let x = 1 :: Float
        y = convert x :: Word8
    print y
    return ()
