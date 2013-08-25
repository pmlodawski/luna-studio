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


main :: IO ()
main = do 
    Parser.main
