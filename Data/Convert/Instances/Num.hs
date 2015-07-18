{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Convert.Instances.Num()
where

import Data.Convert.Base
import Data.Convert.Bound
import Data.Int
import Data.Word
import Data.Convert.Instances.Default ()
import Data.Convert.Instances.TH (numConversions)
import GHC.TypeLits

------------------------------------------------------------
import Data.ByteString (ByteString, singleton)

instance Bounded ByteString where
    minBound = singleton minBound
    maxBound = singleton maxBound


type instance FracBaseOf Integer = Infinite
type instance FracBaseOf Int     = Signed 32
type instance FracBaseOf Int8    = Signed 8
type instance FracBaseOf Int16   = Signed 16
type instance FracBaseOf Int32   = Signed 32
type instance FracBaseOf Int64   = Signed 64

type instance FracBaseOf Word    = Unsigned 32
type instance FracBaseOf Word8   = Unsigned 8
type instance FracBaseOf Word16  = Unsigned 16
type instance FracBaseOf Word32  = Unsigned 32
type instance FracBaseOf Word64  = Unsigned 64

type instance FracBaseOf Float   = Signed 32
type instance FracBaseOf Double  = Signed 64

type instance FracBaseOf Char    = Unsigned 32


--type family FracSafety (a :: FracBase Nat) (b :: FracBase Nat) :: Bool where
--    FracSafety (Signed   a) (Signed   b) = a <=? b
--    FracSafety (Unsigned a) (Signed   b) = (a + 1) <=? b
--    FracSafety (Signed   a) (Unsigned b) = a <=? (b + 1)
--    FracSafety Infinite     Infinite     = True
--    FracSafety a            Infinite     = True
--    FracSafety Infinite     a            = False


--class AutoConvertible a b where
--    autoConvert :: a -> b

--instance (fa ~ FracBaseOf a, fb ~ FracBaseOf b, isSafe ~ FracSafety fa fb, AutoSafeConvert isSafe a b) => AutoConvertible a b where
--    autoConvert = autoSafeConvert (Proxy :: Proxy isSafe)

--class AutoSafeConvert isSafe a b where
--    autoConvert :: Proxy isSafe -> a -> b

--instance AutoSafeConvert True a b where
--    func =



--genConversions numConversions




--instance MaybeConvertible Double Int where
--    tryConvert = boundedConversion (truncate)
--instance Convertible Int Double where
--    convert = fromIntegral

--instance MaybeConvertible Double Int8 where
--    tryConvert = boundedConversion (truncate)
--instance Convertible Int8 Double where
--    convert = fromIntegral

--instance MaybeConvertible Double Int16 where
--    tryConvert = boundedConversion (truncate)
--instance Convertible Int16 Double where
--    convert = fromIntegral

--instance MaybeConvertible Double Int32 where
--    tryConvert = boundedConversion (truncate)
--instance Convertible Int32 Double where
--    convert = fromIntegral

--instance MaybeConvertible Double Int64 where
--    tryConvert = boundedConversion (truncate)
--instance Convertible Int64 Double where
--    convert = fromIntegral

--instance MaybeConvertible Double Word where
--    tryConvert = boundedConversion (truncate)
--instance Convertible Word Double where
--    convert = fromIntegral

--instance MaybeConvertible Double Word8 where
--    tryConvert = boundedConversion (truncate)
--instance Convertible Word8 Double where
--    convert = fromIntegral

--instance MaybeConvertible Double Word16 where
--    tryConvert = boundedConversion (truncate)
--instance Convertible Word16 Double where
--    convert = fromIntegral

--instance MaybeConvertible Double Word32 where
--    tryConvert = boundedConversion (truncate)
--instance Convertible Word32 Double where
--    convert = fromIntegral

--instance MaybeConvertible Double Word64 where
--    tryConvert = boundedConversion (truncate)
--instance Convertible Word64 Double where
--    convert = fromIntegral

--instance MaybeConvertible Float Int where
--    tryConvert = boundedConversion (truncate)
--instance Convertible Int Float where
--    convert = fromIntegral

--instance MaybeConvertible Float Int8 where
--    tryConvert = boundedConversion (truncate)
--instance Convertible Int8 Float where
--    convert = fromIntegral

--instance MaybeConvertible Float Int16 where
--    tryConvert = boundedConversion (truncate)
--instance Convertible Int16 Float where
--    convert = fromIntegral

--instance MaybeConvertible Float Int32 where
--    tryConvert = boundedConversion (truncate)
--instance Convertible Int32 Float where
--    convert = fromIntegral

--instance MaybeConvertible Float Int64 where
--    tryConvert = boundedConversion (truncate)
--instance Convertible Int64 Float where
--    convert = fromIntegral

--instance MaybeConvertible Float Word where
--    tryConvert = boundedConversion (truncate)
--instance Convertible Word Float where
--    convert = fromIntegral

--instance MaybeConvertible Float Word8 where
--    tryConvert = boundedConversion (truncate)
--instance Convertible Word8 Float where
--    convert = fromIntegral

--instance MaybeConvertible Float Word16 where
--    tryConvert = boundedConversion (truncate)
--instance Convertible Word16 Float where
--    convert = fromIntegral

--instance MaybeConvertible Float Word32 where
--    tryConvert = boundedConversion (truncate)
--instance Convertible Word32 Float where
--    convert = fromIntegral

--instance MaybeConvertible Float Word64 where
--    tryConvert = boundedConversion (truncate)
--instance Convertible Word64 Float where
--    convert = fromIntegral

--instance MaybeConvertible Rational Int where
--    tryConvert = boundedConversion (truncate)
--instance Convertible Int Rational where
--    convert = fromIntegral

--instance MaybeConvertible Rational Int8 where
--    tryConvert = boundedConversion (truncate)
--instance Convertible Int8 Rational where
--    convert = fromIntegral

--instance MaybeConvertible Rational Int16 where
--    tryConvert = boundedConversion (truncate)
--instance Convertible Int16 Rational where
--    convert = fromIntegral

--instance MaybeConvertible Rational Int32 where
--    tryConvert = boundedConversion (truncate)
--instance Convertible Int32 Rational where
--    convert = fromIntegral

--instance MaybeConvertible Rational Int64 where
--    tryConvert = boundedConversion (truncate)
--instance Convertible Int64 Rational where
--    convert = fromIntegral

--instance MaybeConvertible Rational Word where
--    tryConvert = boundedConversion (truncate)
--instance Convertible Word Rational where
--    convert = fromIntegral

--instance MaybeConvertible Rational Word8 where
--    tryConvert = boundedConversion (truncate)
--instance Convertible Word8 Rational where
--    convert = fromIntegral

--instance MaybeConvertible Rational Word16 where
--    tryConvert = boundedConversion (truncate)
--instance Convertible Word16 Rational where
--    convert = fromIntegral

--instance MaybeConvertible Rational Word32 where
--    tryConvert = boundedConversion (truncate)
--instance Convertible Word32 Rational where
--    convert = fromIntegral

--instance MaybeConvertible Rational Word64 where
--    tryConvert = boundedConversion (truncate)
--instance Convertible Word64 Rational where
--    convert = fromIntegral


--------------------------------------------------------------
--{- The following instances generated by this code:

--int = ["Int", "Int8", "Int16", "Int32", "Int64", "Word", "Word8", "Word16", "Word32",
--       "Word64"]
--allItems l1 l2 = concatMap (\x -> map (\y -> (x, y)) l1) l2
--work = filter (\(a, b) -> a /= b) (allItems int int)
--printIt (f, i) =
--    "instance MaybeConvertible " ++ f ++ " " ++ i ++ " where \n\
--    \    tryConvert = boundedConversion (fromIntegral)\n"

--printInteger i =
--    "instance MaybeConvertible Integer " ++ i ++ " where \n\
--    \    tryConvert = boundedConversion (fromIntegral)\n\
--    \instance Convertible " ++ i ++ " Integer where \n\
--    \    convert = fromIntegral\n\n"

--main = do mapM_ (putStrLn . printIt) work
--          mapM_ (putStrLn . printInteger) int
---}

--instance MaybeConvertible Int Int8 where
--    tryConvert = boundedConversion (fromIntegral)

--instance MaybeConvertible Int Int16 where
--    tryConvert = boundedConversion (fromIntegral)

--instance MaybeConvertible Int Int32 where
--    tryConvert = boundedConversion (fromIntegral)

--instance Convertible Int Int64 where
--    convert = fromIntegral

--instance Convertible Int Word where
--    convert = fromIntegral

--instance MaybeConvertible Int Word8 where
--    tryConvert = boundedConversion (fromIntegral)

--instance MaybeConvertible Int Word16 where
--    tryConvert = boundedConversion (fromIntegral)

--instance MaybeConvertible Int Word32 where
--    tryConvert = boundedConversion (fromIntegral)

--instance Convertible Int Word64 where
--    convert = fromIntegral

--instance Convertible Int8 Int where
--    convert = fromIntegral

--instance Convertible Int8 Int16 where
--    convert = fromIntegral

--instance Convertible Int8 Int32 where
--    convert = fromIntegral

--instance Convertible Int8 Int64 where
--    convert = fromIntegral

--instance Convertible Int8 Word where
--    convert = fromIntegral

--instance Convertible Int8 Word8 where
--    convert = fromIntegral

--instance Convertible Int8 Word16 where
--    convert = fromIntegral

--instance Convertible Int8 Word32 where
--    convert = fromIntegral

--instance Convertible Int8 Word64 where
--    convert = fromIntegral

--instance Convertible Int16 Int where
--    convert = fromIntegral

--instance MaybeConvertible Int16 Int8 where
--    tryConvert = boundedConversion (fromIntegral)

--instance Convertible Int16 Int32 where
--    convert = fromIntegral

--instance Convertible Int16 Int64 where
--    convert = fromIntegral

--instance Convertible Int16 Word where
--    convert = fromIntegral

--instance MaybeConvertible Int16 Word8 where
--    tryConvert = boundedConversion (fromIntegral)

--instance Convertible Int16 Word16 where
--    convert = fromIntegral

--instance Convertible Int16 Word32 where
--    convert = fromIntegral

--instance Convertible Int16 Word64 where
--    convert = fromIntegral

--instance Convertible Int32 Int where
--    convert = fromIntegral

--instance MaybeConvertible Int32 Int8 where
--    tryConvert = boundedConversion (fromIntegral)

--instance MaybeConvertible Int32 Int16 where
--    tryConvert = boundedConversion (fromIntegral)

--instance Convertible Int32 Int64 where
--    convert =  (fromIntegral)

--instance Convertible Int32 Word where
--    convert =  (fromIntegral)

--instance MaybeConvertible Int32 Word8 where
--    tryConvert = boundedConversion (fromIntegral)

--instance MaybeConvertible Int32 Word16 where
--    tryConvert = boundedConversion (fromIntegral)

--instance Convertible Int32 Word32 where
--    convert = fromIntegral

--instance Convertible Int32 Word64 where
--    convert = fromIntegral

--instance MaybeConvertible Int64 Int where
--    tryConvert = boundedConversion (fromIntegral)

--instance MaybeConvertible Int64 Int8 where
--    tryConvert = boundedConversion (fromIntegral)

--instance MaybeConvertible Int64 Int16 where
--    tryConvert = boundedConversion (fromIntegral)

--instance MaybeConvertible Int64 Int32 where
--    tryConvert = boundedConversion (fromIntegral)

--instance MaybeConvertible Int64 Word where
--    tryConvert = boundedConversion (fromIntegral)

--instance MaybeConvertible Int64 Word8 where
--    tryConvert = boundedConversion (fromIntegral)

--instance MaybeConvertible Int64 Word16 where
--    tryConvert = boundedConversion (fromIntegral)

--instance MaybeConvertible Int64 Word32 where
--    tryConvert = boundedConversion (fromIntegral)

--instance MaybeConvertible Int64 Word64 where
--    tryConvert = boundedConversion (fromIntegral)

--instance MaybeConvertible Word Int where
--    tryConvert = boundedConversion (fromIntegral)

--instance MaybeConvertible Word Int8 where
--    tryConvert = boundedConversion (fromIntegral)

--instance MaybeConvertible Word Int16 where
--    tryConvert = boundedConversion (fromIntegral)

--instance MaybeConvertible Word Int32 where
--    tryConvert = boundedConversion (fromIntegral)

--instance MaybeConvertible Word Int64 where
--    tryConvert = boundedConversion (fromIntegral)

--instance MaybeConvertible Word Word8 where
--    tryConvert = boundedConversion (fromIntegral)

--instance MaybeConvertible Word Word16 where
--    tryConvert = boundedConversion (fromIntegral)

--instance MaybeConvertible Word Word32 where
--    tryConvert = boundedConversion (fromIntegral)

--instance MaybeConvertible Word Word64 where
--    tryConvert = boundedConversion (fromIntegral)

--instance MaybeConvertible Word8 Int where
--    tryConvert = boundedConversion (fromIntegral)

--instance MaybeConvertible Word8 Int8 where
--    tryConvert = boundedConversion (fromIntegral)

--instance MaybeConvertible Word8 Int16 where
--    tryConvert = boundedConversion (fromIntegral)

--instance MaybeConvertible Word8 Int32 where
--    tryConvert = boundedConversion (fromIntegral)

--instance MaybeConvertible Word8 Int64 where
--    tryConvert = boundedConversion (fromIntegral)

--instance MaybeConvertible Word8 Word where
--    tryConvert = boundedConversion (fromIntegral)

--instance MaybeConvertible Word8 Word16 where
--    tryConvert = boundedConversion (fromIntegral)

--instance MaybeConvertible Word8 Word32 where
--    tryConvert = boundedConversion (fromIntegral)

--instance MaybeConvertible Word8 Word64 where
--    tryConvert = boundedConversion (fromIntegral)

--instance MaybeConvertible Word16 Int where
--    tryConvert = boundedConversion (fromIntegral)

--instance MaybeConvertible Word16 Int8 where
--    tryConvert = boundedConversion (fromIntegral)

--instance MaybeConvertible Word16 Int16 where
--    tryConvert = boundedConversion (fromIntegral)

--instance MaybeConvertible Word16 Int32 where
--    tryConvert = boundedConversion (fromIntegral)

--instance MaybeConvertible Word16 Int64 where
--    tryConvert = boundedConversion (fromIntegral)

--instance MaybeConvertible Word16 Word where
--    tryConvert = boundedConversion (fromIntegral)

--instance MaybeConvertible Word16 Word8 where
--    tryConvert = boundedConversion (fromIntegral)

--instance MaybeConvertible Word16 Word32 where
--    tryConvert = boundedConversion (fromIntegral)

--instance MaybeConvertible Word16 Word64 where
--    tryConvert = boundedConversion (fromIntegral)

--instance MaybeConvertible Word32 Int where
--    tryConvert = boundedConversion (fromIntegral)

--instance MaybeConvertible Word32 Int8 where
--    tryConvert = boundedConversion (fromIntegral)

--instance MaybeConvertible Word32 Int16 where
--    tryConvert = boundedConversion (fromIntegral)

--instance MaybeConvertible Word32 Int32 where
--    tryConvert = boundedConversion (fromIntegral)

--instance MaybeConvertible Word32 Int64 where
--    tryConvert = boundedConversion (fromIntegral)

--instance MaybeConvertible Word32 Word where
--    tryConvert = boundedConversion (fromIntegral)

--instance MaybeConvertible Word32 Word8 where
--    tryConvert = boundedConversion (fromIntegral)

--instance MaybeConvertible Word32 Word16 where
--    tryConvert = boundedConversion (fromIntegral)

--instance MaybeConvertible Word32 Word64 where
--    tryConvert = boundedConversion (fromIntegral)

--instance MaybeConvertible Word64 Int where
--    tryConvert = boundedConversion (fromIntegral)

--instance MaybeConvertible Word64 Int8 where
--    tryConvert = boundedConversion (fromIntegral)

--instance MaybeConvertible Word64 Int16 where
--    tryConvert = boundedConversion (fromIntegral)

--instance MaybeConvertible Word64 Int32 where
--    tryConvert = boundedConversion (fromIntegral)

--instance MaybeConvertible Word64 Int64 where
--    tryConvert = boundedConversion (fromIntegral)

--instance MaybeConvertible Word64 Word where
--    tryConvert = boundedConversion (fromIntegral)

--instance MaybeConvertible Word64 Word8 where
--    tryConvert = boundedConversion (fromIntegral)

--instance MaybeConvertible Word64 Word16 where
--    tryConvert = boundedConversion (fromIntegral)

--instance MaybeConvertible Word64 Word32 where
--    tryConvert = boundedConversion (fromIntegral)

--instance MaybeConvertible Integer Int where
--    tryConvert = boundedConversion (fromIntegral)
--instance Convertible Int Integer where
--    convert = fromIntegral


--instance MaybeConvertible Integer Int8 where
--    tryConvert = boundedConversion (fromIntegral)
--instance Convertible Int8 Integer where
--    convert = fromIntegral


--instance MaybeConvertible Integer Int16 where
--    tryConvert = boundedConversion (fromIntegral)
--instance Convertible Int16 Integer where
--    convert = fromIntegral


--instance MaybeConvertible Integer Int32 where
--    tryConvert = boundedConversion (fromIntegral)
--instance Convertible Int32 Integer where
--    convert = fromIntegral


--instance MaybeConvertible Integer Int64 where
--    tryConvert = boundedConversion (fromIntegral)
--instance Convertible Int64 Integer where
--    convert = fromIntegral


--instance MaybeConvertible Integer Word where
--    tryConvert = boundedConversion (fromIntegral)
--instance Convertible Word Integer where
--    convert = fromIntegral


--instance MaybeConvertible Integer Word8 where
--    tryConvert = boundedConversion (fromIntegral)
--instance Convertible Word8 Integer where
--    convert = fromIntegral


--instance MaybeConvertible Integer Word16 where
--    tryConvert = boundedConversion (fromIntegral)
--instance Convertible Word16 Integer where
--    convert = fromIntegral


--instance MaybeConvertible Integer Word32 where
--    tryConvert = boundedConversion (fromIntegral)
--instance Convertible Word32 Integer where
--    convert = fromIntegral


--instance MaybeConvertible Integer Word64 where
--    tryConvert = boundedConversion (fromIntegral)
--instance Convertible Word64 Integer where
--    convert = fromIntegral


--------------------------------------------------------------

--instance Convertible Integer Double where
--    convert = fromIntegral
--instance Convertible Integer Float where
--    convert = fromIntegral
--instance Convertible Integer Rational where
--    convert = fromIntegral
--instance Convertible Double Integer where
--    convert = truncate
--instance Convertible Float Integer where
--    convert = truncate
--instance Convertible Rational Integer where
--    convert = truncate

--instance Convertible Float Double where
--    convert = realToFrac
--instance Convertible Double Float where
--    convert = realToFrac
--instance Convertible Float Rational where
--    convert = toRational
--instance Convertible Rational Float where
--    convert = fromRational
--instance Convertible Double Rational where
--    convert = toRational
--instance Convertible Rational Double where
--    convert = fromRational

--------------------------------------------------------------
--instance Convertible Char Integer where
--    convert = fromIntegral . fromEnum
--instance MaybeConvertible Integer Char where
--    tryConvert = boundedConversion (toEnum . fromIntegral)

--------------------------------------------------------------
--{- These instances generated by:

--int = ["Int", "Int8", "Int16", "Int32", "Int64", "Word", "Word8", "Word16", "Word32",
--       "Word64"]
--printIt i =
--    "instance MaybeConvertible Char " ++ i ++ " where \n\
--    \    tryConvert = boundedConversion (fromIntegral . fromEnum)\n\
--    \instance MaybeConvertible " ++ i ++ " Char where \n\
--    \    tryConvert = boundedConversion (toEnum . fromIntegral)\n\n"

--main = do mapM_ (putStrLn . printIt) int
---}

--instance MaybeConvertible Char Int where
--    tryConvert = boundedConversion (fromIntegral . fromEnum)
--instance MaybeConvertible Int Char where
--    tryConvert = boundedConversion (toEnum . fromIntegral)


--instance MaybeConvertible Char Int8 where
--    tryConvert = boundedConversion (fromIntegral . fromEnum)
--instance MaybeConvertible Int8 Char where
--    tryConvert = boundedConversion (toEnum . fromIntegral)


--instance MaybeConvertible Char Int16 where
--    tryConvert = boundedConversion (fromIntegral . fromEnum)
--instance MaybeConvertible Int16 Char where
--    tryConvert = boundedConversion (toEnum . fromIntegral)


--instance MaybeConvertible Char Int32 where
--    tryConvert = boundedConversion (fromIntegral . fromEnum)
--instance MaybeConvertible Int32 Char where
--    tryConvert = boundedConversion (toEnum . fromIntegral)


--instance MaybeConvertible Char Int64 where
--    tryConvert = boundedConversion (fromIntegral . fromEnum)
--instance MaybeConvertible Int64 Char where
--    tryConvert = boundedConversion (toEnum . fromIntegral)


--instance MaybeConvertible Char Word where
--    tryConvert = boundedConversion (fromIntegral . fromEnum)
--instance MaybeConvertible Word Char where
--    tryConvert = boundedConversion (toEnum . fromIntegral)


--instance MaybeConvertible Char Word8 where
--    tryConvert = boundedConversion (fromIntegral . fromEnum)
--instance MaybeConvertible Word8 Char where
--    tryConvert = boundedConversion (toEnum . fromIntegral)


--instance MaybeConvertible Char Word16 where
--    tryConvert = boundedConversion (fromIntegral . fromEnum)
--instance MaybeConvertible Word16 Char where
--    tryConvert = boundedConversion (toEnum . fromIntegral)


--instance MaybeConvertible Char Word32 where
--    tryConvert = boundedConversion (fromIntegral . fromEnum)
--instance MaybeConvertible Word32 Char where
--    tryConvert = boundedConversion (toEnum . fromIntegral)


--instance MaybeConvertible Char Word64 where
--    tryConvert = boundedConversion (fromIntegral . fromEnum)
--instance MaybeConvertible Word64 Char where
--    tryConvert = boundedConversion (toEnum . fromIntegral)


--instance Convertible Integer Integer where
--    convert = id
