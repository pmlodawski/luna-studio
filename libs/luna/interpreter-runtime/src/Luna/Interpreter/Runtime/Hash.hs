---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE EmptyDataDecls         #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverlappingInstances   #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE UndecidableInstances   #-}

module Luna.Interpreter.Runtime.Hash where

import qualified Data.Hash                         as DHash
import           Data.Word
import qualified Luna.Target.HS.Control.Error.Data as Data
import           Prelude                           hiding (print)

---------------------------------------------------------------------------
---- Based on https://www.haskell.org/haskellwiki/GHC/AdvancedOverlap -----
---------------------------------------------------------------------------

class Hash a where
    hash :: a -> Maybe Word64

class Hash' flag a where
    hash' :: flag -> a -> Maybe Word64

instance (HashPred a flag, Hash' flag a) => Hash a where
    hash = hash' (undefined::flag)


-- overlapping instances are used only for HashPred
class HashPred a flag | a->flag where {}

data HTrue    -- Just two
data HFalse   -- distinct types

instance DHash.Hashable a => Hash' HTrue a where
   hash' _ x = Just $ DHash.asWord64 $ DHash.hash x
instance Hash' HFalse a where
   hash' _ _ = Nothing

-- see http://okmij.org/ftp/Haskell/typecast.html
class TypeCast     a b |   a -> b,   b -> a where typeCast   :: a -> b
class TypeCast'  t a b | t a -> b, t b -> a where typeCast'  :: t -> a -> b
class TypeCast'' t a b | t a -> b, t b -> a where typeCast'' :: t -> a -> b
instance TypeCast'  () a b => TypeCast    a b where typeCast x     = typeCast' () x
instance TypeCast'' t  a b => TypeCast' t a b where typeCast'      = typeCast''
instance TypeCast'' () a a                    where typeCast'' _ x = x


instance TypeCast flag HFalse => HashPred a flag -- Used only if the other instances don't apply

---------------------------------------------------------------------------
---- instances for each Data.Hashable is defined --------------------------
---------------------------------------------------------------------------

instance HashPred Bool   HTrue
instance HashPred Char   HTrue
instance HashPred Int    HTrue
instance HashPred Float  HTrue
instance HashPred Double HTrue
instance HashPred String HTrue
instance HashPred a flag => HashPred [a] flag
instance HashPred a flag => HashPred (Data.Safe a) flag
-- etc ...

---------------------------------------------------------------------------
---- custom DHash.Hashable instances --------------------------------------
---------------------------------------------------------------------------

instance DHash.Hashable a => DHash.Hashable (Data.Safe a) where
    hash (Data.Safe a) = DHash.hash a
-- etc ...
