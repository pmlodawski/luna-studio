---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Flowbox Team <contact@flowbox.io>, 2014
-- Proprietary and confidential
-- Unauthorized copying of this file, via any medium is strictly prohibited
---------------------------------------------------------------------------
module Flowbox.Bus.Data.Flag where

import Text.Read as R

import Flowbox.Prelude



data Flag = Enable
          | Disable
          deriving (Eq, Ord)


instance Show Flag where
    show Enable  = show (1 :: Int)
    show Disable = show (0 :: Int)


instance Read Flag where
    readPrec = do n <- readPrec
                  case n :: Int of
                      1 -> return $ Enable
                      0 -> return $ Disable
                      _ -> R.pfail
