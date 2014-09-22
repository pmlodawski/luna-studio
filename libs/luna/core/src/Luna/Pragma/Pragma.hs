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

--import Data.Default

module Luna.Pragma.Pragma where


import Prelude hiding (lookup)
import Text.Parser.Token

import qualified Data.Map as Map
import Data.Map (Map)
import Type.BaseType (baseOf, BaseType)
import Data.Typeable
import Data.Proxy.Utils (proxyTypeName)
import Data.TypeLevel.Set
import Data.Default
import Control.Applicative hiding (empty)
import Text.Parser.Char (noneOf)
import Text.Parser.Char (CharParsing)

----------------------------------------------------------------------
-- Data types
----------------------------------------------------------------------

data PragmaItem a = PragmaItem { name :: String 
                               , val  :: Maybe a
                               } deriving (Show)

newtype PragmaSet a = PragmaSet a deriving (Show, Functor)

fromPragmaSet (PragmaSet a) = a


----------------------------------------------------------------------
-- Utils
----------------------------------------------------------------------

pragma :: (Typeable ta, BaseType (Proxy a) (Proxy ta)) => a -> PragmaItem a
pragma a = PragmaItem name Nothing where
    name = proxyTypeName $ baseOf a


empty = PragmaSet ()

register p (PragmaSet set) = PragmaSet $ insert (pragma p) set

names = fromPragmaSet . fmap setNames

parsePragma (PragmaSet set) name = fmap PragmaSet $ _parsePragma set name

----------------------------------------------------------------------
-- Type classes
----------------------------------------------------------------------

class Typeable a => Pragma a where
    parse :: (TokenParsing m, CharParsing m, Monad m) => PragmaItem a -> m a

    default parse :: (Monad m, CharParsing m, Read a) => PragmaItem a -> m a
    parse a = read . (name a ++) <$> many (noneOf "\n")

class ParsePragma a where
    _parsePragma :: (Monad m, TokenParsing m) => a -> String -> m a

class PragmaSetNames a where
    setNames :: a -> [String]

----------------------------------------------------------------------
-- Instances
----------------------------------------------------------------------

instance ParsePragma () where
    _parsePragma a _ = return a


instance (ParsePragma xs, Read a, Pragma a) => ParsePragma (PragmaItem a,xs) where
    _parsePragma (x,xs) n = if name x == n 
        then do
            nval <- parse x 
            return (x {val = Just nval}, xs) 
        else do 
            nval <- _parsePragma xs n
            return $ (x,nval)

---

instance PragmaSetNames () where
    setNames _ = []

instance PragmaSetNames xs => PragmaSetNames (PragmaItem a,xs) where
    setNames (x,xs) = name x : setNames xs

---

instance a~() => Default (PragmaSet a) where
    def = empty



--data Pragma1 = Pragma1 Int deriving (Show, Read, Typeable)
--data Pragma2 = Pragma2 Int deriving (Show, Read, Typeable)

--instance Pragma Pragma1
--instance Pragma Pragma2

--instance Default Pragma1 where
--    def = Pragma1 0

--instance Default Pragma2 where
--    def = Pragma2 0



--main = do
--    let --s = insert ("a"::String) $ insert (0::Int) $ empty
--        pmap = (pragma $ Pragma1 0 ,(pragma $ Pragma2 0 ,()))
--        ps = register (pragma (undefined :: Pragma1))
--           $ register (pragma (undefined :: Pragma2))
--           $ empty
--        --pmap3 = register (undefined :: Pragma1) pmap2
--        --pmap = (Map.empty,(Map.empty,())) :: (Map Int Pragma1,(Map Int Pragma2,()))
--    --print $ show $ Pragma1 (4 :: Int)
--    print $ ps
--    --print $ _parsePragma pmap "Pragma1" "Pragma1 1"
--    --print $ proxyTypeName . baseOf $ Pragma1 0