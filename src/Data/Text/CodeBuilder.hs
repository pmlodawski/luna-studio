---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE MultiParamTypeClasses         #-}

module Data.Text.CodeBuilder where

import           Flowbox.Prelude hiding (simple)

import qualified Data.Text.Lazy as Text
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy.Builder as Text
import           Data.Text.Lazy.Builder   (toLazyText, fromLazyText)


----------------------------------------------------------------------
-- Data types
----------------------------------------------------------------------

data CodeType a = Simple  { code :: a }
                | Complex { code :: a }


----------------------------------------------------------------------
-- Type classes
----------------------------------------------------------------------

class CodeBuilder c where
    simple   :: Text.Builder -> c
    complex  :: Text.Builder -> c

class Generator g where
    generate :: CodeBuilder a => g -> a

class ToTextBuilder a where
    toTextBuilder :: a -> Text.Builder


----------------------------------------------------------------------
-- Utils
----------------------------------------------------------------------

simplify :: CodeBuilder a => (CodeType Text.Builder) -> a
simplify = \case
    Simple  c -> simple c
    Complex c -> simple $ "(" <> c <> ")"

simple' :: (ToTextBuilder a, CodeBuilder c) => a -> c
simple'  = simple.toTextBuilder

complex' :: (ToTextBuilder a, CodeBuilder c) => a -> c
complex' = complex.toTextBuilder

genmap :: (Generator g, CodeBuilder a) => [g] -> [a]
genmap = map generate

sgenmap :: (Generator g, CodeBuilder a) => [g] -> [a]
sgenmap = map sgenerate

sgenerate :: (Generator g, CodeBuilder a) => g -> a
sgenerate = simplify.generate

----------------------------------------------------------------------
-- Instances
----------------------------------------------------------------------

-- basic

instance Functor CodeType where
    fmap f builder = builder { code = f $ code builder }

instance Applicative CodeType where
    pure = Simple
    l <*> r = case l of
        Simple f -> case r of
            Simple  v -> Simple  $ f v
            Complex v -> Complex $ f v
        Complex f -> Complex $ f (code r)


-- CodeBuilder

instance CodeBuilder (CodeType Text.Builder) where
    simple  = Simple
    complex = Complex

instance CodeBuilder Text.Builder where
    simple   = id
    complex  = id


-- ToTextBuilder

instance ToTextBuilder Text where
    toTextBuilder = fromLazyText

instance ToTextBuilder String where
    toTextBuilder = fromString

instance ToTextBuilder Text.Builder where
    toTextBuilder = id


-- Convertible

instance Convertible Text Text.Builder where
    convert = fromLazyText

instance Convertible String Text.Builder where
    convert = fromString

instance Convertible Text.Builder Text.Builder where
    convert = id

instance Convertible a b => Convertible [a] [b] where
    convert = fmap convert

