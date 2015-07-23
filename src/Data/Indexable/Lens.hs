{-# LANGUAGE NoMonomorphismRestriction #-}

module Data.Indexable.Lens where

import Data.Indexable.Base
import Control.Lens

--uncheckedIso :: Lens' a b
--uncheckedIso = lens uncheckedSetIdx uncheckedGetIdx