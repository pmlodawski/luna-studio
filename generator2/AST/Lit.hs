{-# LANGUAGE DeriveGeneric             #-}

module AST.Lit where

import GHC.Generics        (Generic)
import Data.Binary

data Lit = CharLit Char | IntLit Int
    deriving (Show, Eq, Generic, Read)

instance Binary Lit