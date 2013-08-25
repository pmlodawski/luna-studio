---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Flowbox.Luna.Parser.Keywords where

import           Prelude                                  hiding (lex)
import           Data.Char                                hiding (Space)
import           Text.ParserCombinators.UU                  
import qualified Text.ParserCombinators.UU.Utils          as Utils
import           Text.ParserCombinators.UU.BasicInstances hiding (Parser)
--import qualified Data.ListLike as LL
--import Text.ParserCombinators.UU.Idioms
--import Text.ParserCombinators.UU.Interleaved


import           Flowbox.Luna.Parser.Utils                  

pKey keyw   = pToken keyw `micro` 1



pDef        = lexeme $ pKey "def"
pClass      = lexeme $ pKey "class"
pInterface  = lexeme $ pKey "interface"

pFrom       = lexeme $ pKey "from"
pImport     = lexeme $ pKey "import"
pAs         = lexeme $ pKey "as"



                     