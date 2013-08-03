---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

{-# LANGUAGE FunctionalDependencies #-}

module Luna.Tools.Serialization where

import           Data.Int


class Serialize a b | a -> b, b -> a where
  encode :: a -> b
  decode :: b -> Either String a 


i32toi :: Int32 -> Int
i32toi = fromIntegral


itoi32 :: Int -> Int32
itoi32 = fromIntegral


convert :: [Either String a] -> Either String [a]
convert []       = Right []
convert [h]      = case h of 
    Left m  -> Left m
    Right a -> Right [a]
convert (h:t) = case h of 
    Left m  -> Left m
    Right a -> case (convert t) of 
        Left m1 -> Left m1
        Right a1 -> Right (a:a1)
