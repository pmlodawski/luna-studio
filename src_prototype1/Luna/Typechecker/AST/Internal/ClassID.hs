{-# LANGUAGE DeriveGeneric #-}

module Luna.Typechecker.AST.Internal.ClassID (
    ClassID(..)
  ) where


import Control.DeepSeq

import GHC.Generics

import Text.Printf


newtype ClassID = ClassID String
  deriving (Eq,Show,Ord,Generic)

instance PrintfArg ClassID where
  formatArg (ClassID m) = formatArg m

instance NFData ClassID