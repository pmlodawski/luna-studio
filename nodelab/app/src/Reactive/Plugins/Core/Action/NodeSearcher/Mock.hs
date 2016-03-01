{-# LANGUAGE OverloadedStrings #-}

module Reactive.Plugins.Core.Action.NodeSearcher.Mock where

import           Utils.PreludePlus            hiding (Item)

import           Data.Map                     (Map)
import qualified Data.Map                     as Map

import           Data.Text.Lazy               (Text)

import           Empire.API.Data.NodeSearcher (Item (..), LunaModule (..))
import           JS.NodeSearcher



-- mockInteger = Module $ LunaModule $ Map.fromList [("integer?", Function),("odd?", Function),("even?", Function),("upto", Function),("downto", Function),("times", Function),("succ", Function),("next", Function),("pred", Function),("chr", Function),("ord", Function),("to_i", Function),("to_int", Function),("floor", Function),("ceil", Function),("truncate", Function),("round", Function),("gcd", Function),("lcm", Function),("gcdlcm", Function),("numerator", Function),("denominator", Function),("to_r", Function),("rationalize", Function)]

mockData :: [(Text, Item)]
mockData    =                           [ ("+",         Function)
                                        , ("-",         Function)
                                        , ("*",         Function)
                                        , ("/",         Function)
                                        , (">",         Function)
                                        , ("==",        Function)
                                        , ("<",         Function)
                                        , ("<=",        Function)
                                        , (">=",        Function)
                                        , ("++",        Function)
                                        , ("toString",  Function)
                                        , ("truncate",  Function)
                                        , ("round",     Function)
                                        , ("floor",     Function)
                                        , ("ceiling",   Function)
                                        , ("length",    Function)
                                        , ("null",      Function)
                                        , ("!",         Function)
                                        , ("head",      Function)
                                        , ("last",      Function)
                                        , ("init",      Function)
                                        , ("tail",      Function)
                                        , ("take",      Function)
                                        , ("drop",      Function)
                                        , ("empty",     Function)
                                        , ("singleton", Function)
                                        , ("replicate", Function)
                                        , ("cons",      Function)
                                        , ("snoc",      Function)
                                        , ("++",        Function)
                                        , ("reverse",   Function)
                                        , ("map",       Function)
                                        , ("filter",    Function)
                                        , ("maximum",   Function)
                                        , ("minimum",   Function)
                                        , ("cos",       Function)
                                        , ("acos",      Function)
                                        , ("cosh",      Function)
                                        , ("asin",      Function)
                                        , ("sinh",      Function)
                                        , ("tan",       Function)
                                        , ("atan",      Function)
                                        , ("tanh",      Function)
                                        , ("abs",       Function)
                                        , ("signum",    Function)
                                        , ("min",       Function)
                                        , ("max",       Function)
                                        , ("gcd",       Function)
                                        , ("lcm",       Function)
                                        , ("div",       Function)
                                        , ("mod",       Function)
                                        , ("pi",        Function)
                                        , ("log,",      Function)
                                        , ("sqrt",      Function)
                                        ]

-- getItemsSearch :: Text -> [QueryResult]
-- getItemsSearch expr = searchInScope mockData expr
--
-- getItemsTree :: Text -> [QueryResult]
-- getItemsTree prefix = moduleItems mockData prefix
--
