{-
Copyright (C) 2009-2011 John Goerzen <jgoerzen@complete.org>

All rights reserved.

For license and copyright information, see the file LICENSE

-}

{- |
   Module     : Data.Convertible.Base
   Copyright  : Copyright (C) 2009-2011 John Goerzen
   License    : BSD3

   Maintainer : John Goerzen <jgoerzen@complete.org>
   Stability  : provisional
   Portability: portable

-}

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Convert.Base where


import Control.Monad.Error
import Data.Typeable

{- | The result of a safe conversion via 'tryConvert'. -}
type ConvertResult a = Either ConvertError a

----------------------------------------------------------------------
-- Conversions
----------------------------------------------------------------------

{- | A typeclass that represents something that can be converted.
A @Convertible a b@ instance represents an @a@ that can be converted to a @b@. -}
class MaybeConvertible a b where
    {- | Convert @a@ to @b@, returning Right on success and Left on error.
       For a simpler interface, see 'convert'. -}
    tryConvert :: a -> ConvertResult b

class Convertible a b where
    convert :: a -> b




unsafeConvert :: MaybeConvertible a b => a -> b
unsafeConvert a =
    case tryConvert a of
      Left  e -> error $ prettyConvertError e
      Right r -> r

{-
{- | Any type can be converted to itself. -}
instance Convertible a a where
    tryConvert x = return x
-}

{-
{- | Lists of any convertible type can be converted. -}
instance Convertible a b => Convertible [a] [b] where
    tryConvert = mapM tryConvert
-}

{- | Convert from one type of data to another.  Raises an exception if there is
an error with the conversion.  For a function that does not raise an exception
in that case, see 'tryConvert'. -}
    --convert :: MaybeConvertible a b => a -> b
    --convert x =
    --    case tryConvert x of
    --      Left e -> error (prettyConvertError e)
    --      Right r -> r

{-
instance Convertible Int Double where
    tryConvert = return . fromIntegral
instance Convertible Double Int where
    tryConvert = return . truncate         -- could do bounds checking here
instance Convertible Integer Double where
    tryConvert = return . fromIntegral
instance Convertible Double Integer where
    tryConvert = return . truncate
-}

----------------------------------------------------------------------
-- Error Handling
----------------------------------------------------------------------

{- | How we indicate that there was an error. -}
data ConvertError = ConvertError {
      convSourceValue :: String,
      convSourceType :: String,
      convDestType :: String,
      convErrorMessage :: String}
                    deriving (Eq, Read, Show)

instance Error ConvertError where
    strMsg x = ConvertError "(unknown)" "(unknown)" "(unknown)" x



convError :: (Show a, Typeable a, Typeable b) =>
             String -> a -> ConvertResult b
convError msg inpval = convError' msg inpval undefined where

    convError' :: (Show a, Typeable a, Typeable b) =>
                   String -> a -> b -> ConvertResult b
    convError' msg inpval retval =
        Left $ ConvertError { convSourceValue  = show inpval
                            , convSourceType   = show . typeOf $ inpval
                            , convDestType     = show . typeOf $ retval
                            , convErrorMessage = msg
                            }

prettyConvertError :: ConvertError -> String
prettyConvertError (ConvertError sv st dt em) =
    "Convertible: error converting source data " ++ sv ++ " of type " ++ st
    ++ " to type " ++ dt ++ ": " ++ em

