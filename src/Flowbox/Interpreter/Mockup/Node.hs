
---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Flowbox Team <contact@flowbox.io>, 2014
-- Proprietary and confidential
-- Unauthorized copying of this file, via any medium is strictly prohibited
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}

module Flowbox.Interpreter.Mockup.Node
( module Flowbox.Interpreter.Mockup.Node
, module X
)
where

import Flowbox.Data.Graph              as X
import Flowbox.Interpreter.Mockup.Type (Type)
import Flowbox.Prelude


type ID   = Int


data Node = Node { _code :: String
                 , _cls  :: Type
                 } deriving (Show, Read)


makeLenses(''Node)
