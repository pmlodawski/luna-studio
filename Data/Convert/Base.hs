{-# LANGUAGE FunctionalDependencies #-}

module Data.Convert.Base where


----------------------------------------------------------------------
-- Conversions
----------------------------------------------------------------------

class MaybeConvertible a e b | a b -> e where
    tryConvert :: a -> Either e b

class Convertible a b where
    convert :: a -> b


unsafeConvert :: Show e => MaybeConvertible a e b => a -> b
unsafeConvert a =
    case tryConvert a of
      Left  e -> error $ show e
      Right r -> r

