{-
Copyright (C) 2009-2011 John Goerzen <jgoerzen@complete.org>

All rights reserved.

For license and copyright information, see the file LICENSE

-}

{- |
   Module     : Data.Convertible.Utils
   Copyright  : Copyright (C) 2009-2011 John Goerzen
   License    : BSD3

   Maintainer : John Goerzen <jgoerzen@complete.org>
   Stability  : provisional
   Portability: portable

-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Convert.Bound where

import Data.Convert.Base
import Data.Typeable
import Language.Haskell.TH hiding (Type, Safety, Safe, Unsafe)
import Data.Monoid
import GHC.TypeLits

{- | Utility function to perform bounds checking as part of a conversion.

Does this be examining the bounds of the destination type, converting to the type of
the source via 'tryConvert', comparing to the source value.  Results in an error
if the conversion is out of bounds. -}
fracLowerConversion :: (Typeable a, Typeable b, Convertible a Integer, Show a)
                    => FracBase2 -> (a -> b) -> (a -> ConvertResult b)
fracLowerConversion dstBase func inp = if illegal then err else return result where
    result    = func inp
    smallest  = minFrac dstBase
    biggest   = maxFrac dstBase
    inp'      = convert inp :: Integer
    illegal   = inp' < smallest || inp' > biggest
    err       = convError ("Input value outside of bounds: " ++ show (smallest, biggest)) inp


boundedConversion :: ( Ord a, Bounded b, Show a, Show b, MaybeConvertible a Integer
                     , MaybeConvertible b Integer, Typeable a, Typeable b )
                  => (a -> b) -> (a -> ConvertResult b)
boundedConversion func inp = if illegal then err else return result where
    result    = func inp
    smallest  = asTypeOf minBound result
    biggest   = asTypeOf maxBound result
    smallest' = unsafeConvert smallest ::Integer
    biggest'  = unsafeConvert biggest  ::Integer
    inp'      = unsafeConvert inp      ::Integer
    illegal   = inp' < smallest' || inp' > biggest'
    err       = convError ("Input value outside of bounds: " ++ show (smallest, biggest)) inp



type family FracBaseOf a :: (FracBase Nat)

data FracBase a = Infinite
                | Signed   a
                | Unsigned a

type FracBaseVal = FracBase Int


class Unlift (a :: k) (b :: *) | a -> b where
    unlift :: Proxy a -> b

instance Unlift Infinite FracBaseVal where
    unlift _ = Infinite

instance KnownNat n => Unlift (Signed n) FracBaseVal where
    unlift _ = Signed . fromIntegral $ natVal (Proxy :: Proxy n)

instance KnownNat n => Unlift (Unsigned n) FracBaseVal where
    unlift _ = Unsigned . fromIntegral $ natVal (Proxy :: Proxy n)

--instance Unlift Infinite FracBase where
--    unlift _ = Infinite

--type instance FracBase Int8 = 32


data FracBase2 = Signed2   Int
               | Unsigned2 Int
               deriving (Show)

class Num a => FractionBound a where
    minFrac :: FracBase2 -> a
    maxFrac :: FracBase2 -> a

instance Num a => FractionBound a where
    minFrac = \case
        Signed2   b -> - (2 ^ (b - 1))
        Unsigned2 b -> - (2 ^ b)
    maxFrac = \case
        Signed2   b -> 2 ^ (b - 1) - 1
        Unsigned2 b -> 2 ^ b - 1

data BoundError = BoundError

--generateBoundConversions :: ((String, [Int]), (String, [Int])) -> Q [Dec]
--generateBoundConversions ((tpName, sorts), (tpName', sorts')) = return [] where
--    names =

data Safety = Safe
            | Unsafe
            deriving (Show)


data Conversion = Conversion Safety (Q Exp) FractionType FractionType

instance Show Conversion where
    show (Conversion t _ a b) = "Conversion " <> show t <> " " <> name a <> " " <> name b


data Uni t = Uni { fromfunc :: String
                 , tp       :: t
                 } deriving (Show, Functor)

data FractionLayout  = FractionLayout Base
                  deriving (Show)


data ComplexLayout = ComplexLayout
                   deriving (Show)

data Type layout = Type { name   :: String
                        , layout :: layout
                        } deriving (Show)

type FractionType = Type FractionLayout

--class IsType a where
--    typeName :: a -> String

--instance IsType BitType where
--    typeName = name

--genBitConversionsWith :: String -> [BitType] -> [BitType] -> [Conversion]
--genBitConversionsWith f bs bs' = genBitConversionWith f <$> bs <*> bs' where

--genBitConversionWith :: String -> BitType -> BitType -> Conversion
--genBitConversionWith f (BitType name sort) (BitType name' sort') = Conversion sec f name name' where
--    sec = if sort > sort' then Unsafe else Safe


--genBitConversions :: Uni [BitType] -> Uni [BitType] -> [Conversion]
--genBitConversions (Uni f bt) (Uni _ bt') = genBitConversionsWith f bt bt'

data Base = Min
          | Base Int
          | Max
          deriving (Show, Ord, Eq)

instance Num Base where
    fromInteger = Base . fromInteger
    abs = \case
        Min    -> Min
        Max    -> Max
        Base a -> Base $ abs a


fractionals :: [String] -> [Base] -> [FractionType]
fractionals = zipWith fractional'

fractional' :: String -> Base -> FractionType
fractional' name sort = Type name (FractionLayout sort)

fractional :: String -> Base -> [FractionType]
fractional name sort = return $ fractional' name sort

--UnsafeConversion = Conversion Unsafe
--SafeConversion   = Conversion Safe

class MkConversion a b where
    mkConversion :: Q Exp -> a -> b -> Conversion

mkConversions :: MkConversion a b => Q Exp -> [a] -> [b] -> [Conversion]
mkConversions f a b = mkConversion f <$> a <*> b

safeConversion :: Q Exp -> FractionType -> FractionType -> Conversion
safeConversion f = Conversion Safe f

unsafeConversion :: Q Exp -> FractionType -> FractionType -> Conversion
unsafeConversion f = Conversion Unsafe f

safeConversions :: Q Exp -> [FractionType] -> [FractionType] -> [Conversion]
safeConversions f a b = safeConversion f <$> a <*> b

unsafeConversions :: Q Exp -> [FractionType] -> [FractionType] -> [Conversion]
unsafeConversions f a b = unsafeConversion f <$> a <*> b


instance MkConversion FractionType FractionType where
    mkConversion f t@(Type name (FractionLayout base)) t'@(Type name' (FractionLayout base')) = Conversion saf f t t' where
        saf = if base > base' then Unsafe else Safe



sortNames :: [Int] -> String -> [String]
sortNames sorts name = fmap (mappend name . show) sorts

fracSorts :: Num a => [a]
fracSorts = [8,16,32,64]

fracNames :: String -> [String]
fracNames = sortNames fracSorts


genConversion :: Conversion -> Q Dec
genConversion c@(Conversion tp qexp t t') = undefined -- do
    --exp <- qexp :: Q Exp
    --let tname  = mkName $ name t
    --    tname' = mkName $ name t'
    --    (cname, fname, fmod) = case tp of
    --            Safe   -> ("Convertible"     , "convert"   , id)
    --            Unsafe -> ("MaybeConvertible", "tryConvert", AppE ([|fracLowerConversion (layout t')|]))
    --    convf = [ValD (VarP $ mkName fname) (NormalB $ fmod exp) []]

    --return $ InstanceD []
    --         (AppT (AppT (ConT $ mkName cname) (ConT tname)) (ConT tname')) convf

--genConversion :: Conversion -> Q Dec
--genConversion (Conversion tp qexp ident ident') = do
--    exp <- qexp
--    return $ InstanceD []
--             (AppT (AppT (ConT (mkName "MaybeConvertible")) (ConT name)) (ConT name'))
--             [ValD (VarP (mkName "tryConvert")) (NormalB exp) []]
--    where name  = mkName ident
--          name' = mkName ident'


apps = foldl AppT

genConversions :: [Conversion] -> Q [Dec]
genConversions = mapM genConversion

main = do
    --print $ conversions


    --print $ mkConversions "truncate"     floats words

    --print $ (mkConversion "truncate"
    ----print $ genBitConversions (Uni "truncate" [doubleType]) intFamily
    --print $ fmap (UnsafeConversion "truncate" "Double" . typeName)  FractionTypes
    --print $ fmap (flip (SafeConversion "fromIntegral") "Double" . typeName)  FractionTypes
    print "end"

genSortNames :: Convertible a String => String -> [a] -> [String]
genSortNames base lst = fmap (mappend base . convert) lst

instance Convertible Int String where
    convert = show

{- | Useful for defining conversions that are implemented in terms of other
conversions via an intermediary type. Instead of:

>instance MaybeConvertible CalendarTime POSIXTime where
>    tryConvert a = do r <- tryConvert a
>                       tryConvert (r :: ClockTime)

we can now write:

>instance MaybeConvertible CalendarTime POSIXTime where
>    tryConvert = convertVia (undefined::ClockTime)

which does the same thing -- converts a CalendarTime to a ClockTime, then a
ClockTime to a POSIXTime, both using existing 'MaybeConvertible' instances.
 -}
convertVia :: (MaybeConvertible a b, MaybeConvertible b c) =>
              b                 -- ^ Dummy data to establish intermediate type - can be undefined
           -> a                 -- ^ Input value
           -> ConvertResult c   -- ^ Result
convertVia dummy inp =
    do r1 <- tryConvert inp
       tryConvert (asTypeOf r1 dummy)
