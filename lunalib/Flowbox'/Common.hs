{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Flowbox'.Common where

import Data.Tuple.OneTuple
import Common'.C''add

instance C''add (Int,Int) (OneTuple Int) where
	add (x, y) = OneTuple $ x+y
	add''M  = return . add

