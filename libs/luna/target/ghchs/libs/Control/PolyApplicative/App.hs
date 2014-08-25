{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell           #-}

module Control.PolyApplicative.App where

import Control.Category.Dot
import Control.PolyApplicative
import Control.PolyApplicative.App.TH

app1 f a = f <<*>> a

mkApp 2 10
