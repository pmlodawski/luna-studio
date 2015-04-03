{-# LANGUAGE DeriveGeneric             #-}

module Generator.AST.Lit where
	
import GHC.Generics        (Generic)

data Lit = CharLit Char | IntLit Int
    deriving (Show, Eq, Generic, Read)
