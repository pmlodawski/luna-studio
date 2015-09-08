---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Main where

import Control.Exception.Assert

import Flowbox.Animation.Controller
import Flowbox.Prelude



main :: IO ()
main = do
  putStrLn "---------------- Testing non numeric animations ---------------"
  let key = (0 :: Float)
      value = "string 0"
      animation = insertEmpty (insertValue (create key value) 50 "string 1") 100
  putStrLn $ assert ((valueAt animation 10 )==(Just ("string 0"))) "---------------- test 1 - ok --------------------" 
  putStrLn $ assert ((valueAt animation 60 )==(Just ("string 1"))) "---------------- test 2 - ok --------------------" 
  putStrLn $ assert ((valueAt animation 110 )==(Nothing)) "---------------- test 3 - ok --------------------" 
  return ()
