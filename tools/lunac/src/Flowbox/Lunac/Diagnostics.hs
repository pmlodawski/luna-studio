---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE NoMonomorphismRestriction #-}

module Flowbox.Lunac.Diagnostics where

import           Control.Monad
import           Flowbox.Prelude            
import qualified Flowbox.Text.Show.Pretty as PP
import           Flowbox.System.Log.Logger   

data Diagnostics = Diagnostics { printAst :: Bool 
                               }


logger :: Logger
logger = getLogger "Flowbox.Lunac.Diagnostics"

printAST ast diag = when (printAst diag) $ logger info (PP.ppqShow ast)
