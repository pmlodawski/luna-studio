---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell           #-}

module Luna.Build.Diagnostics where

import Data.Text.Lazy (unpack)

import Flowbox.Prelude
import Flowbox.System.Log.Logger
import Flowbox.Text.Show.Hs      (hsShow)



logger :: LoggerIO
logger = getLoggerIO $moduleName


data Diagnostics = Diagnostics { showAST  :: Bool
                               , showSA   :: Bool
                               , showSSA  :: Bool
                               , showHash :: Bool
                               , showHAST :: Bool
                               , showHSC  :: Bool
                               }


header :: String -> String
header txt = "\n-------- " <> txt <> " --------"


printHeader :: MonadIO m => String -> m ()
printHeader = logger debug . header


all :: Diagnostics
all = Diagnostics True True True True True True


none :: Diagnostics
none = Diagnostics False False False False False False


printCond :: (MonadIO m, Show s) => Bool -> s -> m ()
printCond cond v = when cond $  prettyPrint v


printAST, printSA, printSSA, printHash, printHAST
    :: (MonadIO m, Show s) => Diagnostics -> s -> m ()
printAST  diag = printCond (showAST  diag)
printSA   diag = printCond (showSA   diag)
printSSA  diag = printCond (showSSA  diag)
printHash diag = printCond (showHash diag)
printHAST diag = printCond (showHAST diag)

printHSC :: MonadIO m => Diagnostics -> Text -> m ()
printHSC  diag hsc = when (showHSC  diag) $ putStrLn (hsShow $ unpack hsc)
