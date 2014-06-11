---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Flowbox.Tools.Serialize.Proto.Conversion.List where

import qualified Data.Foldable                  as Foldable
import qualified Data.Sequence                  as Sequence
import           Data.Sequence                    (Seq)

import           Flowbox.Prelude                  
import           Flowbox.Tools.Conversion.Proto   



encodeList :: Convert a b => [a] -> Seq b
encodeList = Sequence.fromList . map encode


decodeList :: (Convert a b, Applicative m, Monad m) => Seq b -> m [a]
decodeList = mapM decode . Foldable.toList


encodeListP :: ConvertPure a b => [a] -> Seq b
encodeListP = Sequence.fromList . map encodeP


decodeListP :: ConvertPure a b => Seq b -> [a]
decodeListP = map decodeP . Foldable.toList