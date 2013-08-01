---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Luna.Tools.Serialization.Types where

import           Data.Int
import           Data.HashTable
import qualified Data.Text.Lazy as Text
import qualified Data.Vector    as Vector
import           Data.Vector      (Vector)

import qualified Types_Types
import           Luna.Tools.Serialization
import           Luna.Type.Type        (Type(..))

type2typeProtoList :: Int -> Type -> [Types_Types.TypeProto]
type2typeProtoList level t = case t of 
    Undefined       -> [s] where
                       s = Types_Types.TypeProto (Just Types_Types.Undefined) Nothing Nothing Nothing Nothing Nothing Nothing
    Module name     -> [s] where
                       s = Types_Types.TypeProto (Just Types_Types.Module) (Just $ Text.pack name) Nothing Nothing Nothing Nothing Nothing 
    List items      -> s:(type2typeProtoList (level+1) items) where
                       s = Types_Types.TypeProto (Just Types_Types.List) Nothing Nothing Nothing Nothing Nothing (Just $hashInt $ level + 1)


instance Serialize Type Types_Types.Type where
  encode t = tc where list = type2typeProtoList 0 t
                      tc = Types_Types.Type $ Just $ Vector.fromList list
  decode a = undefined
