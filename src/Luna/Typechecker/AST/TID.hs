{-# LANGUAGE DeriveGeneric #-}

module Luna.Typechecker.AST.TID (
    enumTID, TID(..)
  ) where


import Control.DeepSeq

import GHC.Generics

import Text.Printf


newtype TID = TID String
  deriving (Eq,Show,Ord,Generic)

instance PrintfArg TID where
  formatArg (TID m) = formatArg m

instance NFData TID


enumTID :: Int -> TID
enumTID n = TID $ "v" ++ show n
