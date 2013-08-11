{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Flowbox.Luna.Libs.Std.Base where

import Flowbox.Luna.FClasses.U'add

instance C''add' (Int,(Int,())) (Int,()) where
	add' (x, (y,_)) = (x+y,())
	add'''M  = return . add'

