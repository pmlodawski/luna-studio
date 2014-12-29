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
-- data ImplicitSelf = ImplicitSelf
--                   deriving (Show, Read, Typeable)
-- 
-- implicitSelf = pragma :: SwitchPragma ImplicitSelf
-- 
-- main = do
--     let ps = enablePragma implicitSelf
--            $ registerPragma implicitSelf
--            $ (def :: PragmaSet)
-- 
--     print $ lookupPragma implicitSelf ps
-- @
--
-- Pragmas with arguments are supported by default.
----------------------------------------------------------------------------

module Luna.System.Pragma where

import           Flowbox.Prelude hiding (noneOf)

import qualified Data.HTSet.HTSet        as HTSet
import           Data.HTSet.HTSet        (HTSet)
import qualified Data.HMap               as HMap
import           Data.Typeable
import           Control.Monad.State
import           Prelude                 ()
import           Text.Parser.Char        (string, noneOf, CharParsing)
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

type family PragmaVal t a :: *

type    ParseCtx    m = (TokenParsing m, CharParsing m, Monad m)
newtype ParseRule t a = ParseRule { fromRule :: ParseCtx m => Pragma t a -> m a }
data    Pragma    t a = Pragma    { _name    :: String
                                  , _parser  :: ParseRule t a
                                  } deriving (Typeable)
makeLenses ''Pragma

-- Tells what should happen when pragma is succesfully parsed
class PragmaCons t where
    pragmaCons :: Pragma t a -> a -> PragmaVal t a


-- == Utils ==

pragma :: (Typeable a, Read a) => Pragma t a
pragma = def :: (Read a, Typeable a) => Pragma t a

-- == Instances ==

instance (val ~ PragmaVal t a) => HTSet.IsKey (Pragma t a) (PragmaStack val)

instance Typeable a => Show (Pragma t a) where
    show a = "Pragma " ++ show (view name a)

instance (Typeable a, Read a) => Default (Pragma t a) where
    def = Pragma { _name   = takeWhile (/= ' ') . show . typeOf $ (undefined :: a)
                 , _parser = ParseRule $ (\a -> (read .: (++)) <$> string (view name a) <*> many (noneOf "\n"))
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

----------------------------------------------------------------------
-- PragmaSet
----------------------------------------------------------------------

newtype PragmaSetTransform = PragmaSetTransform { unSetTrans :: (Monad m, TokenParsing m) => m (PragmaSet -> PragmaSet) }
data PragmaSet = PragmaSet { _values  :: HTSet
                           , _parsers :: Map String PragmaSetTransform
                           }

makeLenses ''PragmaSet

type PragmaSetCtx t a s rep = (HasPragmaSet s, rep~PragmaVal t a, Typeable rep)

-- == Utils ==

pragmaNames :: PragmaSet -> [String]
pragmaNames = Map.keys . view parsers

-- == Instances ==

instance Default PragmaSet where
    def = PragmaSet def def

-- remove when makeClassy gets fixed (bug: https://github.com/ekmett/lens/issues/512) --
class HasPragmaSet t where
    pragmaSet :: Simple Lens t PragmaSet
instance HasPragmaSet PragmaSet where pragmaSet = id
-- / --

instance Show PragmaSet where
    show ps = "PragmaSet " ++ show (pragmaNames ps)


-- == Utils ==

-- For now all parsed pragmas are pushed to the stack 
createSetTransform :: (rep~PragmaVal t a, PragmaCons t, Typeable rep) => Pragma t a -> PragmaSetTransform
createSetTransform p = PragmaSetTransform $ do
    a <- fromRule (p^.parser) p
    return $ pushPragma p (pragmaCons p a)

parsePragmas :: HasPragmaSet s => s -> PragmaSetTransform
parsePragmas s = PragmaSetTransform $ foldr (<|>) (fail "No matching pragma definition") . fmap (try.unSetTrans) . Map.elems . view (pragmaSet.parsers) $ s

lookupPragmaStack :: PragmaSetCtx t a s rep => Pragma t a -> s -> Maybe (PragmaStack rep)
lookupPragmaStack p = HTSet.lookupKey p . view (pragmaSet.values)

setPragma :: PragmaSetCtx t a s rep => Pragma t a -> rep -> s -> s
setPragma p a = setPragmaStack p (PragmaStack [a])

pushPragma :: PragmaSetCtx t a s rep => Pragma t a -> rep -> s -> s
pushPragma p a ps = flip (setPragmaStack p) ps $ case lookupPragmaStack p ps of
    Nothing -> fromList [a]
    Just s  -> fromList $ a : toList s


pushPragma2 :: PragmaSetCtx t a s rep => Pragma t a -> rep -> s -> Maybe s
pushPragma2 p a ps = case lookupPragmaStack p ps of
    Nothing -> Nothing
    Just s  -> Just $ setPragmaStack p (fromList $ a : toList s) ps

popPragma :: PragmaSetCtx t a s rep => Pragma t a -> s -> Lookup (rep, s)
popPragma p ps = case lookupPragmaStack p ps of
    Nothing -> Unregistered
    Just s  -> case toList s of
        []   -> Undefined
        x:xs -> Defined (x, setPragmaStack p (fromList xs) ps)

setPragmaStack :: PragmaSetCtx t a s rep => Pragma t a -> PragmaStack rep -> s -> s
setPragmaStack p a = pragmaSet.values %~ HTSet.insertKey p a

lookupPragma :: PragmaSetCtx t a s rep => Pragma t a -> s -> Lookup rep
lookupPragma p ps = case lookupPragmaStack p ps of
    Nothing -> Unregistered
    Just s  -> case toList s of
        []   -> Undefined
        x:xs -> Defined x

registerPragma :: (PragmaSetCtx t a s rep, PragmaCons t) => Pragma t a -> s -> s
registerPragma p s = setPragmaStack p mempty s
                   & (pragmaSet.parsers) %~ Map.insert (p^.name) (createSetTransform p)


----------------------------------------------------------------------
-- SwitchPragma
----------------------------------------------------------------------

data Switch         = Enabled | Disabled deriving (Show, Typeable, Eq, Ord)
type SwitchPragma a = Pragma Switch a

-- == Utils ==

enablePragma :: (HasPragmaSet s, Typeable a) => SwitchPragma a -> s -> s
enablePragma p = pushPragma p Enabled

disablePragma :: (HasPragmaSet s, Typeable a) => SwitchPragma a -> s -> s
disablePragma p = pushPragma p Disabled

-- == Instances ==

instance Default Switch where
    def = Enabled

instance PragmaCons Switch where
    pragmaCons _ = const Enabled

type instance PragmaVal Switch a = Switch


----------------------------------------------------------------------
-- ValPragma
----------------------------------------------------------------------

data Val
type ValPragma a = Pragma Val a

-- == Instances ==

instance PragmaCons Val where
    pragmaCons _ = id

type instance PragmaVal Val a = a