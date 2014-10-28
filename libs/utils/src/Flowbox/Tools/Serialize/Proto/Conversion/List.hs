---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE MultiParamTypeClasses #-}

module Flowbox.Tools.Serialize.Proto.Conversion.List where

import           Control.Monad.Trans.Either
import qualified Data.Foldable              as Foldable
import           Data.Sequence              (Seq)
import qualified Data.Sequence              as Sequence

import Flowbox.Prelude
import Flowbox.Tools.Conversion.Proto



encodeList :: Convert a b => [a] -> Seq b
encodeList = Sequence.fromList . map encode


decodeList :: Convert a b => Seq b -> Either String [a]
decodeList = mapM decode . Foldable.toList


decodeListE :: (Monad m, Convert a b) => Seq b -> EitherT String m [a]
decodeListE = hoistEither . decodeList


encodeListP :: ConvertPure a b => [a] -> Seq b
encodeListP = Sequence.fromList . map encodeP


decodeListP :: ConvertPure a b => Seq b -> [a]
decodeListP = map decodeP . Foldable.toList
