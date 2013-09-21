---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts, ConstraintKinds #-}

module Flowbox.Luna.Passes.General.Print.Print where

import qualified Flowbox.Luna.Passes.Pass  as Pass
import           Flowbox.Luna.Passes.Pass    (PassMonad)

import           Flowbox.System.Log.Logger   
import           Flowbox.Prelude           hiding (error, id)

logger :: Logger
logger = getLogger "Flowbox.Luna.Passes.HSPrint.HSPrint"

type SSAMonad m = PassMonad Pass.NoState m


indent :: Int
indent = 4


run :: PassMonad s m => String -> Pass.Result m String
run s = return $ hsprint 0 s


hsprint :: Int -> String -> String
hsprint _ []     = []
hsprint i (x:xs) = case x of
	'{' -> x : newline (i+1) ++     hsprint (i+1) xs
	';' -> x : newline (i)   ++     hsprint i xs
	',' -> x : newline (i)   ++     hsprint i xs
	'}' ->     newline (i-1) ++ x : hsprint (i-1) xs
	_   -> x : hsprint i xs


newline :: Int -> String
newline i = "\n" ++ replicate (indent*i) ' '