{-# LANGUAGE TypeOperators #-}

module Luna.Syntax.AST.Decl.Function where

import Prologue
import Luna.Syntax.Model.Graph

data Function n = Function { _self :: Maybe (Ref $ Node n)
                           , _args :: [Ref $ Node n]
                           , _out  :: Ref $ Node n
                           , _body :: Graph n (Link n)
                           } deriving (Show)
makeLenses ''Function
