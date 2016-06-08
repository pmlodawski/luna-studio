{-# LANGUAGE OverloadedStrings #-}
module Reactive.Commands.Node.Ports.Colors
    ( colorPort
    , vtToColor
    ) where

import           Utils.PreludePlus
import           Data.Hashable                 (hash)

import qualified Empire.API.Data.Port                as Port
import           Empire.API.Data.ValueType           (ValueType (..))
import           Empire.API.Data.TypeRep       (TypeRep (..))

hashMany :: [TypeRep] -> Int
hashMany as = sum $ zipWith (*) powers (tpRepToColor <$> as) where
    powers = (37 ^) <$> [0..]

ensureRange n = (n `mod` 8) + 1

tpRepToColor :: TypeRep -> Int
tpRepToColor (TCons tn as) = ensureRange $ case tn of
     "Int"        -> 0
     "Bool"       -> 1
     "Double"     -> 2
     "String"     -> 3
     "List"       -> 5 + hashMany as
     _            -> hash tn + hashMany as
tpRepToColor (TLam as out) = ensureRange . hashMany $ out : as
tpRepToColor (TVar n) = 9
tpRepToColor _ = 0

vtToColor (TypeIdent t) = tpRepToColor t
vtToColor _ = 0

colorPort port = vtToColor $ port ^. Port.valueType
