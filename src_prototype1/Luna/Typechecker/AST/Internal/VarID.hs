{-# LANGUAGE DeriveGeneric #-}

module Luna.Typechecker.AST.Internal.VarID (
    VarID(..)
  ) where


import Control.DeepSeq

import GHC.Generics

import Text.Printf


newtype VarID = VarID String
  deriving (Eq,Show,Ord,Generic)

instance PrintfArg VarID where
  formatArg (VarID m) = formatArg m

instance NFData VarID