{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Flowbox.Luna.Helpers.Common where

import Flowbox.Luna.FClasses.C''add'

instance C''add' (Int,(Int,())) (Int,()) where
	add' (x, (y,_)) = (x+y,())
	add'''M  = return . add'

