---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE NoMonomorphismRestriction #-}

module Flowbox.Lunac.Diagnostics where

import           Control.Monad               (when)
import           Flowbox.Prelude             
import qualified Flowbox.Text.Show.Pretty  as PP
import           Flowbox.System.Log.Logger   
import           Flowbox.Text.Show.Hs        (hsShow)
import qualified Flowbox.Luna.Data.Source  as Source
import           Data.String.Utils           (join)

data Diagnostics = Diagnostics { showAST  :: Bool 
                               , showVA   :: Bool
                               , showSSA  :: Bool
                               , showHAST :: Bool
                               , showHSC  :: Bool
                               }


logger :: Logger
logger = getLogger "Flowbox.Lunac.Diagnostics"

printAST  v diag = when (showAST  diag) $ logger info (PP.ppqShow v)
printVA   v diag = when (showVA   diag) $ logger info (PP.ppShow  v)
printSSA  v diag = when (showSSA  diag) $ logger info (PP.ppqShow v)
printHAST v diag = when (showHAST diag) $ logger info (PP.ppShow  v)
printHSC  v diag = when (showHSC  diag) $ logger info (showSrcs   v)


showSrcs srcs = join "\n\n" $ map showSrc srcs 

showSrc src   = ">>> file '" ++ join "/" (Source.path src) ++ "':\n\n"
              ++ hsShow (Source.code src)