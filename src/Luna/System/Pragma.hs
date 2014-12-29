{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Luna.System.Pragma
-- Copyright   :  (C) 2014 Flowbox
-- License     :  AllRightsReserved
-- Maintainer  :  Wojciech Daniło <wojciech.danilo@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- Usage:
--
-- Pragmas can be defined as standalone data types and can be registered
-- in PragmaSet's. Each PragmaSet contains PragmaStack, which handles
-- such operations as push or pop for pragmas.
--
-- Each pragma defines its name, which is automatically taken from type name
-- if not provided manually and parsing rules
--
-- @
-- data ImplicitSelf a = ImplicitSelf Int 
--                     deriving (Show, Read, Typeable)
-- 
-- implicitSelf = pragma :: Pragma (ImplicitSelf Int)
-- 
-- main = do
--     let ps = pushPragma implicitSelf (ImplicitSelf 5)
--            $ registerPragma implicitSelf
--            $ (def :: PragmaSet)
-- 
--     print $ popPragma implicitSelf ps
-- @
----------------------------------------------------------------------------

module Luna.System.Pragma where

import           Flowbox.Prelude hiding (noneOf)

import qualified Data.HTSet.HTSet        as HTSet
import           Data.HTSet.HTSet        (HTSet)
import qualified Data.HMap               as HMap
import           Data.Typeable
import           Control.Monad.State
import           Prelude                 ()
import           Text.Parser.Char        (noneOf, CharParsing)
import           Text.Parser.Token
import           Text.Parser.Combinators (try)
import           Data.Proxy.Utils
import qualified Data.Map                as Map
import           Data.Map                (Map)


----------------------------------------------------------------------
-- Lookup
----------------------------------------------------------------------

data Lookup a = Defined a
              | Undefined
              | Unregistered
              deriving (Show, Functor, Foldable)

----------------------------------------------------------------------
-- Pragma
----------------------------------------------------------------------

data Pragma a = Pragma { _name  :: String
                       , _parse :: (TokenParsing m, CharParsing m, Monad m) => Pragma a -> m a
                       } deriving (Typeable)
makeLenses ''Pragma

pragma :: (Typeable a, Read a) => Pragma a
pragma = def :: (Read a, Typeable a) => Pragma a

-- == Instances ==

instance Typeable a => Show (Pragma a) where
    show a = "Pragma " ++ show (view name a)

instance (Typeable a, Read a) => Default (Pragma a) where
    def = Pragma { _name   = takeWhile (/= ' ') . show . typeOf $ (undefined :: a)
                 , _parse = (\a -> read . ((view name a) ++) <$> many (noneOf "\n"))
                 }


----------------------------------------------------------------------
-- PragmaStack
----------------------------------------------------------------------

newtype PragmaStack a = PragmaStack [a] deriving (Show, Typeable, Default, Monoid, Functor)

-- == Instances ==

instance IsList (PragmaStack a) where
    type Item (PragmaStack a) = a
    toList (PragmaStack a)    = a
    fromList                  = PragmaStack

instance HTSet.IsKey (Pragma a) (PragmaStack a)

----------------------------------------------------------------------
-- PragmaSet
----------------------------------------------------------------------

type PragmaSetTransform m = m (PragmaSet -> PragmaSet)
data PragmaSet = PragmaSet { _values  :: HTSet
                           , _parsers :: (Monad m, TokenParsing m) => Map String (PragmaSetTransform m)
                           }

makeLenses ''PragmaSet

type PragmaSetCtx s a = (HasPragmaSet s, Typeable a)


-- == Instances ==

instance Default PragmaSet where
    def = PragmaSet def def

-- remove when makeClassy gets fixed (bug: https://github.com/ekmett/lens/issues/512) --
class HasPragmaSet t where
    pragmaSet :: Simple Lens t PragmaSet
instance HasPragmaSet PragmaSet where pragmaSet = id
-- / --

instance Show PragmaSet where
    show ps = "PragmaSet " -- ++ show (pragmaNames ps)


-- == Utils ==

parsePragmas :: (HasPragmaSet s, Monad m, TokenParsing m) => s -> PragmaSetTransform m
parsePragmas = foldr (<|>) (fail "No matching pragma definition") . fmap try . Map.elems . view (pragmaSet.parsers)

lookupPragmaStack :: PragmaSetCtx s a => Pragma a -> s -> Maybe (PragmaStack a)
lookupPragmaStack p = HTSet.lookupKey p . view (pragmaSet.values)

setPragma :: PragmaSetCtx s a => Pragma a -> a -> s -> s
setPragma p a = setPragmaStack p (PragmaStack [a])

pushPragma :: PragmaSetCtx s a => Pragma a -> a -> s -> s
pushPragma p a ps = flip (setPragmaStack p) ps $ case lookupPragmaStack p ps of
    Nothing -> fromList [a]
    Just s  -> fromList $ a : toList s

popPragma :: PragmaSetCtx s a => Pragma a -> s -> Lookup (a, s)
popPragma p ps = case lookupPragmaStack p ps of
    Nothing -> Unregistered
    Just s  -> case toList s of
        []   -> Undefined
        x:xs -> Defined (x, setPragmaStack p (fromList xs) ps)

setPragmaStack :: PragmaSetCtx s a => Pragma a -> PragmaStack a -> s -> s
setPragmaStack p a = pragmaSet.values %~ HTSet.insertKey p a

lookupPragma :: PragmaSetCtx s a => Pragma a -> s -> Lookup a
lookupPragma p ps = case lookupPragmaStack p ps of
    Nothing -> Unregistered
    Just s  -> case toList s of
        []   -> Undefined
        x:xs -> Defined x

registerPragma :: PragmaSetCtx s a => Pragma a -> s -> s
registerPragma p = setPragmaStack p mempty
