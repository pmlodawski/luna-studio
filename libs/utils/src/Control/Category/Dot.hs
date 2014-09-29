{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell           #-}

module Control.Category.Dot where

import Control.Category.Dot.TH
import Prelude

dot0  = ($)
dot1  = (.)

mkDots 2 10
