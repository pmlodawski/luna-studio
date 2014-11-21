---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module Flowbox.Tools.Conversion.Proto where

import Control.Monad.Trans.Either

import Flowbox.Control.Error
import Flowbox.Prelude



type Error = String


missing :: String -> String -> Error
missing datatype field = concat ["Failed to decode ", datatype, ": '", field, "'is missing"]


class Convert a b | a -> b where
    encode :: a -> b
    decode :: b -> Either Error a

    decodeE :: Monad m => b -> EitherT Error m a
    decodeE = hoistEither . decode

    encodeJ :: a -> Maybe b
    encodeJ = Just . encode

    decodeJ :: Maybe b -> Error -> Either Error a
    decodeJ b e = decode =<< b <?> e

    decodeJE :: Monad m => Maybe b -> Error -> EitherT Error m a
    decodeJE = hoistEither .: decodeJ


class ConvertPure a b | a -> b where
    encodeP :: a -> b
    decodeP :: b -> a

    encodePJ :: a -> Maybe b
    encodePJ = Just . encodeP

    decodePJ :: Maybe b -> Error -> Either Error a
    decodePJ b e = decodeP <$> b <?> e

    decodePJE :: Monad m => Maybe b -> Error -> EitherT Error m a
    decodePJE = hoistEither .: decodePJ
