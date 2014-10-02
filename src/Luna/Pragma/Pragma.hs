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
import           Data.Map (Map)
import           Type.BaseType (baseOf, BaseType)
import           Data.Typeable
import           Data.Proxy.Utils (proxyTypeName)
import qualified Data.TypeLevel.Set as Set
import           Data.Default
import           Control.Applicative hiding (empty)
import           Text.Parser.Char (noneOf)
import           Text.Parser.Char (CharParsing)

----------------------------------------------------------------------
-- Data types
----------------------------------------------------------------------

data Pragma a = Pragma { name :: String 
                       , val  :: Maybe a
                       } deriving (Show)

newtype PragmaSet a = PragmaSet a deriving (Show, Functor)

fromPragmaSet (PragmaSet a) = a

data Lookup a = Defined a
              | Undefined
              | Unregistered
              deriving (Show)


----------------------------------------------------------------------
-- Utils
----------------------------------------------------------------------

pragma :: (Typeable ta, BaseType (Proxy a) (Proxy ta)) => a -> Pragma a
pragma a = Pragma (baseName a) Nothing where


baseName = proxyTypeName . baseOf

empty = PragmaSet ()

register p (PragmaSet set) = PragmaSet $ Set.insert (pragma p) set

set p (PragmaSet set) = PragmaSet $ Set.modify (\(Pragma name _) -> Pragma name (Just p)) set

lookup (PragmaSet set) = case Set.lookup set of
        Nothing              -> Unregistered
        Just (Pragma name p) -> case p of
            Nothing -> Undefined
            Just x  -> Defined x

names = fromPragmaSet . fmap setNames

parsePragma (PragmaSet set) name = fmap PragmaSet $ parseByName set name

----------------------------------------------------------------------
-- Type classes
----------------------------------------------------------------------

class Typeable a => IsPragma a where
    parse :: (TokenParsing m, CharParsing m, Monad m) => Pragma a -> m a

    default parse :: (Monad m, CharParsing m, Read a) => Pragma a -> m a
    parse a = read . (name a ++) <$> many (noneOf "\n")

class ParsePragma a where
    parseByName :: (Monad m, TokenParsing m) => a -> String -> m a

class PragmaSetNames a where
    setNames :: a -> [String]

----------------------------------------------------------------------
-- Instances
----------------------------------------------------------------------

instance ParsePragma () where
    parseByName a _ = return a


instance (ParsePragma xs, Read a, IsPragma a) => ParsePragma (Pragma a,xs) where
    parseByName (x,xs) n = if name x == n 
        then do
            nval <- parse x 
            return (x {val = Just nval}, xs) 
        else do 
            nval <- parseByName xs n
            return $ (x,nval)

---

instance PragmaSetNames () where
    setNames _ = []

instance PragmaSetNames xs => PragmaSetNames (Pragma a,xs) where
    setNames (x,xs) = name x : setNames xs

---

instance a~() => Default (PragmaSet a) where
    def = empty



--data Pragma1 = Pragma1 Int deriving (Show, Read, Typeable)
--data Pragma2 = Pragma2 Int deriving (Show, Read, Typeable)
--data Pragma3 = Pragma3 Int deriving (Show, Read, Typeable)

--instance IsPragma Pragma1
--instance IsPragma Pragma2

--instance Default Pragma1 where
--    def = Pragma1 0

--instance Default Pragma2 where
--    def = Pragma2 0






--main = do
--    let --s = insert ("a"::String) $ insert (0::Int) $ empty
--        pmap = (pragma $ Pragma1 0 ,(pragma $ Pragma2 0 ,()))
--        ps = register (undefined :: Pragma1)
--           $ register (undefined :: Pragma2)
--           $ empty

--        ps2 = set (Pragma1 5) ps

--    case lookup ps2 of
--        Unregistered        -> print "Pragma not defined!"
--        Undefined           -> print "Pragma not set"
--        Defined (Pragma2 x) -> print $ "found " ++ show x

--    --case lookup ps2 of
--    --    Nothing -> print $ "Pragma '" ++ baseName (Unregistered :: Pragma1) ++ "' not defined"
--    --    Just (Pragma name Nothing)            -> print "not set"
--    --    Just (Pragma name (Just (Pragma1 x))) -> print $ "found " ++ show x
--        --pmap3 = register (Unregistered :: Pragma1) pmap2
--        --pmap = (Map.empty,(Map.empty,())) :: (Map Int Pragma1,(Map Int Pragma2,()))
--    --print $ show $ Pragma1 (4 :: Int)

--    print $ ps
--    --print $ parseByName pmap "Pragma1" "Pragma1 1"
--    --print $ proxyTypeName . baseOf $ Pragma1 0