---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}

module Flowbox.Luna.Data.Graph.Node where

import Flowbox.Prelude



data Node = Expr     { _expr :: String, _outputName :: String, _x :: Float, _y :: Float }
          | Inputs   {                                         _x :: Float, _y :: Float }
          | Outputs  {                                         _x :: Float, _y :: Float }
          deriving (Show, Eq)


makeLenses (''Node)


type ID = Int


position :: Node -> (Float, Float)
position node = (node ^. x, node ^. y)


position' :: (ID, Node) -> (Float, Float)
position' = position . snd
