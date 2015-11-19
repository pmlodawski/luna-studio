---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}

module Type.BaseType where

import Data.Typeable
import Data.Proxy.Utils
import Prelude

--------------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------------
 
baseOf :: BaseType (Proxy a) b => a -> b
baseOf = baseType . toProxy

--------------------------------------------------------------------------------
-- Type classes
--------------------------------------------------------------------------------
 
class BaseType a b | a -> b where
    baseType :: a -> b

--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------
 
instance out~(Proxy b) => BaseType (Proxy b) out where
    baseType _ = Proxy

instance ((BaseType (Proxy b) (Proxy x)), out~Proxy x) => BaseType (Proxy (b t)) out where
    baseType _ = Proxy

-- TODO: czemu nie mozna zrefaktoryzowac tego wyzej? Wtedy nie dizala!
-- np. baseType $ toProxy $ Just (5::Int)

--data X a = X a deriving (Show)

--class Test a b | a -> b where
--    test :: a -> b

--instance Test (X a) Int where
--    test _ = 5

--main = do
--    print $ typeOf $ baseType (undefined :: Proxy (Maybe Bool))
--    print $ typeOf $ baseType (undefined :: Proxy (Either Bool String))
--    print $ typeOf $ baseOf (Just True)
    --print $ typeOf $ baseOf (Just return)

    --print $ test (X 1)


    
