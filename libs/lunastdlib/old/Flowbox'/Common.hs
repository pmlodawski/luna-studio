{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Flowbox'.Common where

import Data.Tuple.OneTuple
import Common'.C''add

instance C''add (Int,(Int,())) (Int,()) where
	add (x, (y,_)) = (x+y,())
	add''M  = return . add

