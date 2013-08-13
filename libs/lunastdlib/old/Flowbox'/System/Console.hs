{-# LANGUAGE TemplateHaskell, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}

module Flowbox'.System.Console where

import           Common'.C''print   
import           Flowbox'.Core      
import qualified Prelude            
import           Prelude            (return, Show, show)


data Console = Console deriving (Show)


print'T''M (cons@(Console{}), (s,())) = do
	Prelude.print s
	return (cons,())

print'T (cons@(Console{}), (s,())) = let
    x = show s
	in (cons,())

mkInst'' ''C''print 'print'T 'print'T''M 'print




