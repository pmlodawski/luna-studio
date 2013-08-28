---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

import           Debug.Trace                  

import qualified Flowbox.Luna.Parser.Parser as Parser
import qualified Flowbox.Luna.Parser.Lexer  as Lexer
import qualified Text.Show.Pretty           as PP
import           System.TimeIt                


example :: String
example = unlines [ "a\nb\nc"
                  ]

main :: IO ()
main = do
    timeIt main_inner

main_inner :: IO ()
main_inner = do
    let out = Parser.parse example
    print out
    putStrLn $ PP.ppShow $ out