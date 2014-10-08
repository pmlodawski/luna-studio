---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Luna.Pass.Build.Diagnostics where

import Control.Monad.IO.Class
import Data.String.Utils      (join)

import qualified Flowbox.Generics.Deriving.QShow as QShow
import           Flowbox.Prelude
import           Flowbox.System.Log.Logger
import           Flowbox.Text.Show.Hs            (hsShow)
import qualified Flowbox.Text.Show.Pretty        as PP
import           Luna.Data.AliasInfo             (AliasInfo)
import qualified Luna.Data.AliasInfo             as AliasInfo
import           Luna.Data.Source                (Source)
import qualified Luna.Data.Source                as Source



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
    logger info "\n>> scope:"
    logger info $ PP.ppShow (v ^. AliasInfo.scope)
    logger info "\n>> alias map:"
    logger info $ PP.ppShow (v ^. AliasInfo.alias)
    logger info "\n>> orphans map:"
    logger info $ PP.ppShow (v ^. AliasInfo.orphans)


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
showSrc src = ">>> file '" ++ join "/" (src ^. Source.path) ++ "':\n\n"
             ++ hsShow (src ^. Source.code)
