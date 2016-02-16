{-# LANGUAGE TypeOperators #-}

module Luna.Syntax.AST.Decl.Function where

import Prologue
import Data.Graph.Backend.VectorGraph


data FunctionPtr n = FunctionPtr { _self :: Maybe (Ref Node n)
                                 , _args :: [Ref Node n]
                                 , _out  :: Ref Node n
                                 } deriving (Show)
makeLenses ''FunctionPtr

data Function n g = Function { _fptr  :: FunctionPtr n
                             , _graph :: g
                             } deriving (Show)
makeLenses ''Function

data Lambda n = Lambda { _lptr     :: FunctionPtr n
                       , _subgraph :: Ref Cluster SubGraph
                       } deriving (Show)

makeLenses ''Lambda
