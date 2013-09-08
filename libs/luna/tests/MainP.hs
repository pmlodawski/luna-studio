---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

import           Flowbox.Prelude                      
import           Debug.Trace                          

import qualified Flowbox.Luna.Passes.Txt2AST.Parser as Parser
import qualified Flowbox.Luna.Passes.Txt2AST.Lexer  as Lexer
import qualified Text.Show.Pretty                   as PP
import qualified Flowbox.Luna.Data.Source           as Source
import           Flowbox.Luna.Data.Source             (Source)
import           System.TimeIt                        


example :: Source
example = Source.Source ["Workspace"] 
        $ unlines [ "def f(x):"
                  , "   def g(y):"
                  , "       f = x + y"
                  ]

main :: IO ()
main = do
    timeIt main_inner

main_inner :: IO ()
main_inner = do
    let out = Parser.parse example
    print out
    putStrLn $ PP.ppShow $ out