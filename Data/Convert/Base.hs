{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ConstraintKinds #-}

module Data.Convert.Base where


----------------------------------------------------------------------
-- Conversions
----------------------------------------------------------------------

class MaybeConvertible a e b | a b -> e where
    tryConvert :: a -> Either e b

class Convertible a b where
    convert :: a -> b

type IsoConvertible a b = (Convertible a b, Convertible b a)

unsafeConvert :: Show e => MaybeConvertible a e b => a -> b
unsafeConvert a =
    case tryConvert a of
      Left  e -> error $ show e
      Right r -> r


--instance {-# OVERLAPPABLE #-} Convertible a a where
--    convert = id

instance {-# OVERLAPPABLE #-} Convertible a b => Convertible (Maybe a) (Maybe b) where
    convert = fmap convert
