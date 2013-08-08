---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Luna.Network.Graph.DefaultValue(
DefaultValue(..)
) where

import qualified Data.Serialize       as Serialize
import           Data.Serialize         (Serialize)
import           Data.Word              (Word8)

data DefaultValue = DefaultInt Int
                  | DefaultString String 
                  deriving (Show, Read, Ord, Eq)



------------------------- INSTANCES -------------------------


instance Serialize DefaultValue where
  put i = case i of 
            DefaultInt     value'   -> Serialize.put (0 :: Word8, value')
            DefaultString  value'   -> Serialize.put (1 :: Word8, value')

  get   = do 
            t <- Serialize.get :: Serialize.Get Word8
            case t of 
              0 -> do value'        <- Serialize.get; return $ DefaultInt    value'
              1 -> do value'        <- Serialize.get; return $ DefaultString value'
              _ -> error "Unknown DefaultValue Type (unserialize)"