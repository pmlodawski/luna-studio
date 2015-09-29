{-# LANGUAGE OverloadedStrings #-}

module Reactive.Plugins.Core.Action.NodeSearcher.Mock where

import           Utils.PreludePlus

import qualified Data.Map as Map
import           Data.Map                   ( Map )

import           Data.Text.Lazy             ( Text )

import           JS.NodeSearcher
import           Reactive.Plugins.Core.Action.NodeSearcher.Scope


mockInteger = Module $ LunaModule $ Map.fromList [("integer?", Function),("odd?", Function),("even?", Function),("upto", Function),("downto", Function),("times", Function),("succ", Function),("next", Function),("pred", Function),("chr", Function),("ord", Function),("to_i", Function),("to_int", Function),("floor", Function),("ceil", Function),("truncate", Function),("round", Function),("gcd", Function),("lcm", Function),("gcdlcm", Function),("numerator", Function),("denominator", Function),("to_r", Function),("rationalize", Function)]

mockData    = LunaModule $ Map.fromList [("Integer", mockInteger)]

getItemsSearch :: Text -> [QueryResult]
getItemsSearch expr = searchInScope mockData expr

getItemsTree :: Text -> [QueryResult]
getItemsTree prefix = moduleItems mockData prefix

