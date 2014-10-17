---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Control.PolyApplicative.App where

import Control.Category.Dot
import Control.PolyApplicative
import Control.PolyApplicative.App.TH



app1 f a = f <<*>> a

mkApp 2 10
