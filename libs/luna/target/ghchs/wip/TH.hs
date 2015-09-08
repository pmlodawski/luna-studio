{-# LANGUAGE NoMonomorphismRestriction #-}
module TH where

import Language.Haskell.TH


two = return $ LitE (IntegerL 2)
