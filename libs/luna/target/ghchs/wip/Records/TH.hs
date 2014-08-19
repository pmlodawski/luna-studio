{-# LANGUAGE NoMonomorphismRestriction #-}

module TH where

import Language.Haskell.TH


emptyTuple = mkName "()"

nop = return $ [ValD (ConP emptyTuple []) (NormalB (ConE emptyTuple)) []]