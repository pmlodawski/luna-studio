---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Flowbox.Data.Convert where

import           Data.ByteString.Lazy       (ByteString)
import qualified Data.ByteString.Lazy.Char8 as ByteString
import qualified Data.Foldable              as Foldable
import           Data.Int                   (Int32, Int64)
import           Data.IntSet                (IntSet)
import qualified Data.IntSet                as IntSet
import           Data.Sequence              (Seq)
import qualified Data.Sequence              as Sequence
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import           Data.Time.Clock            (NominalDiffTime)
import           Foreign.C.Types            (CTime)
import           System.Posix.Types         (FileOffset)

import           Flowbox.Control.Error
import           Flowbox.Prelude            hiding (Text)
import           Flowbox.System.UniPath     (UniPath)
import qualified Flowbox.System.UniPath     as UniPath
import qualified Text.ProtocolBuffers.Basic as Proto



type Error = String


missing :: String -> String -> Error
missing datatype field = concat ["Failed to decode ", datatype, ": '", field, "'is missing"]


class Convert a b where
    encode :: a -> b
    decode :: b -> Either Error a

    decodeE :: Monad m => b -> ExceptT Error m a
    decodeE = hoistEither . decode

    encodeJ :: a -> Maybe b
    encodeJ = Just . encode

    decodeJ :: Maybe b -> Error -> Either Error a
    decodeJ b e = decode =<< b <?> e

    decodeJE :: Monad m => Maybe b -> Error -> ExceptT Error m a
    decodeJE = hoistEither .: decodeJ

    decodeE' :: Monad m => b -> m a
    decodeE' = eitherToM . decode


class ConvertPure a b where
    encodeP :: a -> b
    decodeP :: b -> a

    encodePJ :: a -> Maybe b
    encodePJ = Just . encodeP

    decodePJ :: Maybe b -> Error -> Either Error a
    decodePJ b e = decodeP <$> b <?> e

    decodePJE :: Monad m => Maybe b -> Error -> ExceptT Error m a
    decodePJE = hoistEither .: decodePJ


instance ConvertPure Int Int32 where
    encodeP = fromIntegral
    decodeP = fromIntegral


instance Convert a b => Convert [a] (Seq b) where
    encode = Sequence.fromList . map encode
    decode = mapM decode . Foldable.toList


instance ConvertPure a b => ConvertPure [a] (Seq b) where
    encodeP = Sequence.fromList . map encodeP
    decodeP = map decodeP . Foldable.toList


instance ConvertPure Int b => ConvertPure IntSet (Seq b) where
    encodeP = encodeP . IntSet.toList
    decodeP = IntSet.fromList . decodeP


instance ConvertPure String Proto.Utf8 where
    encodeP = Proto.uFromString
    decodeP = Proto.uToString


instance ConvertPure Text Proto.Utf8 where
    encodeP = encodeP . Text.unpack
    decodeP = Text.pack . decodeP


instance ConvertPure UniPath Proto.Utf8 where
    encodeP = encodeP . UniPath.toUnixString
    decodeP = UniPath.fromUnixString . decodeP


instance ConvertPure CTime Int64 where
    encodeP = fromIntegral . fromEnum
    decodeP = fromIntegral


instance ConvertPure FileOffset Int64 where
    encodeP = fromIntegral
    decodeP = fromIntegral


instance ConvertPure NominalDiffTime Int64 where
    encodeP = fromIntegral . fromEnum
    decodeP = fromIntegral


instance ConvertPure String ByteString where
    encodeP = ByteString.pack
    decodeP = ByteString.unpack
