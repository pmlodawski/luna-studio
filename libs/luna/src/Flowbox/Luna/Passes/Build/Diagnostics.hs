---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Flowbox.Luna.Passes.Build.Diagnostics where

import Control.Monad          (when)
import Control.Monad.IO.Class
import Data.String.Utils      (join)


import qualified Flowbox.Generics.Deriving.QShow as QShow
import qualified Flowbox.Luna.Data.Pass.Source   as Source
import           Flowbox.Prelude
import           Flowbox.System.Log.Logger
import qualified Flowbox.Text.Show.Pretty        as PP


data Diagnostics = Diagnostics { showDM   :: Bool
                               , showAST  :: Bool
                               , showVA   :: Bool
                               , showFP   :: Bool
                               , showSSA  :: Bool
                               , showHAST :: Bool
                               , showHSC  :: Bool
                               }


all :: Diagnostics
all = Diagnostics True True True True True True True


none :: Diagnostics
none = Diagnostics False False False False False False False


logger :: LoggerIO
logger = getLoggerIO "Flowbox.Luna.Passes.Build.Diagnostics"


printDM :: (Show a, MonadIO m) => a -> Diagnostics -> m ()
printDM   v diag = when (showDM   diag) $ logger info (PP.ppShow  v)


printAST :: (QShow.QShow a, MonadIO m) => a -> Diagnostics -> m ()
printAST  v diag = when (showAST  diag) $ logger info (PP.ppqShow v)


printVA :: (Show a, MonadIO m) => a -> Diagnostics -> m ()
printVA   v diag = when (showVA   diag) $ logger info (PP.ppShow  v)


printFP :: (Show a, MonadIO m) => a -> Diagnostics -> m ()
printFP   v diag = when (showFP   diag) $ logger info (PP.ppShow  v)


printSSA :: (QShow.QShow a, MonadIO m) => a -> Diagnostics -> m ()
printSSA  v diag = when (showSSA  diag) $ logger info (PP.ppqShow v)


printHAST :: (Show a, MonadIO m) => a -> Diagnostics -> m ()
printHAST v diag = when (showHAST diag) $ logger info (PP.ppShow  v)


printHSC :: MonadIO m => [Source.Source] -> Diagnostics -> m ()
printHSC  v diag = when (showHSC  diag) $ logger info (showSrcs   v)


showSrcs :: [Source.Source] -> String
showSrcs srcs = join "\n\n" $ map showSrc srcs


showSrc :: Source.Source -> String
showSrc src   = ">>> file '" ++ join "/" (Source.path src) ++ "':\n\n"
              ++ (Source.code src)
