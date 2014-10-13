{-# LANGUAGE DeriveGeneric #-}

module Luna.Typechecker.AST.IDs (
    TyID(..), VarID(..)
  ) where


import Control.DeepSeq

import GHC.Generics


newtype VarID = VarID String
  deriving (Eq,Show,Ord,Generic)

newtype TyID = TID String
  deriving (Eq,Show,Ord,Generic)

