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

import Flowbox.Prelude



class Convert a b | a -> b where
    encode :: a -> b
    decode :: b -> Either String a

    decodeE :: Monad m => b -> EitherT String m a
    decodeE = hoistEither . decode

    encodeJ :: a -> Maybe b
    encodeJ = Just . encode


class ConvertPure a b | a -> b where
    encodeP :: a -> b
    decodeP :: b -> a

    encodePJ :: a -> Maybe b
    encodePJ = Just . encodeP
