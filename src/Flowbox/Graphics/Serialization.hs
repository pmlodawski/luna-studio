---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Flowbox.Graphics.Serialization where

import Flowbox.Graphics.Prelude

import qualified Flowbox.Math.Matrix as M
import qualified Generated.Proto.Graphics.Matrix           as Proto
import qualified Generated.Proto.Graphics.Matrix.Precision as Proto
import Data.Array.Accelerate.IO
import Data.ByteString.Lazy



class MatrixSerialize a where
    serializeMatrix :: M.Matrix2 a -> IO (Maybe Proto.Matrix)

instance MatrixSerialize Float where
    serializeMatrix (M.Raw arr) = do
        ((), mat) <- toByteString arr
        return $ Just $ Proto.Matrix Proto.SINGLE (fromStrict mat)

    serializeMatrix _ = return Nothing

instance MatrixSerialize Double where
    serializeMatrix (M.Raw arr) = do
        ((), mat) <- toByteString arr
        return $ Just $ Proto.Matrix Proto.DOUBLE (fromStrict mat)

    serializeMatrix _ = return Nothing
