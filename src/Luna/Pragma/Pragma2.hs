---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
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
--{-# LANGUAGE PolyKinds #-}

--import Data.Default

module Luna.Pragma.Pragma2 where

import qualified Data.HTSet.HTSet as HTSet
import           Data.HTSet.HTSet (HTSet)
import Flowbox.Prelude hiding (noneOf)
--import Data.HMap
import qualified Data.HMap as HMap
import Data.Typeable
import Control.Monad.State
import Prelude ()
import           Text.Parser.Char (noneOf, CharParsing)
import Text.Parser.Token
import Text.Parser.Combinators (try)

import Data.Proxy.Utils

import qualified Data.Map as Map
import           Data.Map (Map)



pragma = def :: (Read a, Typeable a) => Pragma a


data Pragma a = Pragma { _name  :: String
                       , _parse :: (TokenParsing m, CharParsing m, Monad m) => Pragma a -> m a
                       } deriving (Typeable)

makeLenses ''Pragma



newtype PragmaStack a = PragmaStack [a] deriving (Show, Typeable, Default, Monoid, Functor)

data PragmaSet = PragmaSet { _values  :: HTSet
                           , _parsers :: (Monad m, TokenParsing m) => Map String (PragmaSetTransform m)
                           }

type PragmaSetTransform m = m (PragmaSet -> PragmaSet)





makeLenses ''PragmaSet

instance Show PragmaSet where
    show ps = "PragmaSet " -- ++ show (pragmaNames ps)


instance IsList (PragmaStack a) where
    type Item (PragmaStack a) = a
    toList (PragmaStack a)    = a
    fromList                  = PragmaStack

instance HTSet.IsKey (Pragma a) (PragmaStack a)

instance Typeable a => Show (Pragma a) where
    show a = "Pragma " ++ show (view name a)

instance (Typeable a, Read a) => Default (Pragma a) where
    def = Pragma { _name   = takeWhile (/= ' ') . show . typeOf $ (undefined :: a)
                 , _parse = (\a -> read . ((view name a) ++) <$> many (noneOf "\n"))
                 }



data ImplicitSelf a = ImplicitSelf Int deriving (Show, Read, Typeable)

implicitSelf = pragma :: Pragma (ImplicitSelf Int)



parsePragmas :: (Monad m, TokenParsing m) => PragmaSet -> PragmaSetTransform m
parsePragmas = foldr (<|>) (fail "No matching pragma definition") . fmap try . Map.elems . view parsers

mkPragmaSetTransform :: (Monad m, TokenParsing m, Typeable a) => Pragma a -> PragmaSetTransform m
mkPragmaSetTransform p = do
    val <- (view parse p) p
    return $ (values %~ HTSet.insert p)

data Session = Session

class (MonadIO m) => LunaMonad m where
    getSession :: m Session
    setSession :: Session -> m ()



pragmaMap :: (Monad m, TokenParsing m) => Map String (PragmaSetTransform m)
pragmaMap = mempty




instance Default PragmaSet where
    def = PragmaSet def def

--setPragma :: Typeable a => Pragma a -> a -> PragmaSet -> PragmaSet
--setPragma p a = values %~ HTSet.insertKey p a

lookupPragmaStack :: Typeable a => Pragma a -> PragmaSet -> Maybe (PragmaStack a)
lookupPragmaStack p = HTSet.lookupKey p . view values

setPragma :: Typeable a => Pragma a -> a -> PragmaSet -> PragmaSet
setPragma p a = setPragmaStack p (PragmaStack [a])

pushPragma :: Typeable a => Pragma a -> a -> PragmaSet -> PragmaSet
pushPragma p a ps = flip (setPragmaStack p) ps $ case lookupPragmaStack p ps of
    Nothing -> fromList [a]
    Just s  -> fromList $ a : toList s

popPragma :: Typeable a => Pragma a -> PragmaSet -> Lookup (a, PragmaSet)
popPragma p ps = case lookupPragmaStack p ps of
    Nothing -> Unregistered
    Just s  -> case toList s of
        []   -> Undefined
        x:xs -> Defined (x, setPragmaStack p (fromList xs) ps)

setPragmaStack :: Typeable a => Pragma a -> PragmaStack a -> PragmaSet -> PragmaSet
setPragmaStack p a = values %~ HTSet.insertKey p a


lookupPragma :: Typeable a => Pragma a -> PragmaSet -> Lookup a
lookupPragma p ps = case lookupPragmaStack p ps of
    Nothing -> Unregistered
    Just s  -> case toList s of
        []   -> Undefined
        x:xs -> Defined x

data Lookup a = Defined a
              | Undefined
              | Unregistered
              deriving (Show)

registerPragma p = setPragmaStack p mempty



main = do
    let ps = pushPragma implicitSelf (ImplicitSelf 5)
           $ pushPragma implicitSelf (ImplicitSelf 6)
           $ registerPragma implicitSelf
           $ (def :: PragmaSet)

    print $ popPragma implicitSelf ps




    print $ implicitSelf
    --let a = undefined :: BaseType3 (Proxy (A Int))
        --b = a :: Proxy A
    --print $ typeOf (undefined :: BaseType3 (Proxy (A Int)))
    --print $ typeOf (undefined :: (Proxy A))
    --let m = insert (5::Int) mempty :: HTSet
    print "end"