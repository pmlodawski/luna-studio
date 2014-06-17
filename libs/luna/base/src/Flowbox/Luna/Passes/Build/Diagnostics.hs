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

import qualified Flowbox.Generics.Deriving.QShow  as QShow
import           Flowbox.Luna.Data.Pass.AliasInfo (AliasInfo)
import qualified Flowbox.Luna.Data.Pass.AliasInfo as AliasInfo
import           Flowbox.Luna.Data.Pass.Source    (Source)
import qualified Flowbox.Luna.Data.Pass.Source    as Source
import           Flowbox.Prelude
import           Flowbox.System.Log.Logger
import           Flowbox.Text.Show.Hs             (hsShow)
import qualified Flowbox.Text.Show.Pretty         as PP



data Diagnostics = Diagnostics { showAST  :: Bool
                               , showAA   :: Bool
                               , showSSA  :: Bool
                               , showHash :: Bool
                               , showHAST :: Bool
                               , showHSC  :: Bool
                               }


all :: Diagnostics
all = Diagnostics True True True True True True


none :: Diagnostics
none = Diagnostics False False False False False False


logger :: LoggerIO
logger = getLoggerIO "Flowbox.Luna.Passes.Build.Diagnostics"


printAST :: (QShow.QShow a, MonadIO m) => a -> Diagnostics -> m ()
printAST v diag = when (showAST  diag) $ logger info (PP.ppqShow v)


printAA :: MonadIO m => AliasInfo -> Diagnostics -> m ()
printAA v diag = when (showAA   diag) $ do
    logger info "\n>> varRel:"
    logger info $ PP.ppShow (v ^. AliasInfo.varRel)
    logger info "\n>> aliasMap:"
    logger info $ PP.ppShow (v ^. AliasInfo.aliasMap)
    logger info "\n>> invalidMap:"
    logger info $ PP.ppShow (v ^. AliasInfo.invalidMap)


printSSA :: (QShow.QShow a, MonadIO m) => a -> Diagnostics -> m ()
printSSA  v diag = when (showSSA  diag) $ logger info (PP.ppqShow v)


printHash :: (Show a, MonadIO m) => a -> Diagnostics -> m ()
printHash v diag = when (showHash diag) $ logger info (PP.ppShow  v)


printHAST :: (Show a, MonadIO m) => a -> Diagnostics -> m ()
printHAST v diag = when (showHAST diag) $ logger info (PP.ppShow  v)


printHSC :: MonadIO m => [Source] -> Diagnostics -> m ()
printHSC  v diag = when (showHSC  diag) $ logger info (showSrcs   v)


showSrcs :: [Source] -> String
showSrcs srcs = join "\n\n" $ map showSrc srcs


showSrc :: Source -> String
showSrc src = ">>> file '" ++ join "/" (Source.path src) ++ "':\n\n"
             ++ hsShow (Source.code src)
