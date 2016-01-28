{-# LANGUAGE UndecidableInstances #-}

module Luna.Syntax.AST.Decl.Function where

import Prologue
import Luna.Syntax.Repr.Graph (Ref, Node)

data Function a = Function { _self :: Maybe (Ref Node)
                           , _args :: [Ref Node]
                           , _out  :: Ref Node
                           , _body :: a
                           } deriving (Show, Eq)
makeLenses ''Function
