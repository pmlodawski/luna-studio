---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
#endif

module Flowbox.Generics.Deriving.FShow (
  -- * Generic show class
    FShow(..)

  -- * Default definition
  , fshowsPrecdefault

  ) where


import           Generics.Deriving.Base        
import           Generics.Deriving.Instances   ()

import           Flowbox.Prelude             hiding (from)


type FType = String->String

--------------------------------------------------------------------------------
-- Generic show
--------------------------------------------------------------------------------

appPrec :: Int
appPrec = 2

data Type = Rec | Tup | Pref | Inf String

class FShow' m where
  fshowsPrec' :: Type -> Int -> FType -> m a -> ShowS
  isNullary   :: m a -> Bool
  isNullary = error "generic show (isNullary): unnecessary case"

instance FShow' U1 where
  fshowsPrec' _ _ _ U1 = id
  isNullary _ = True

instance (FShow c) => FShow' (K1 i c) where
  fshowsPrec' _ n f (K1 a) = fshowsPrec n f a
  isNullary _ = False

-- No instances for P or Rec because fshow is only applicable to types of kind *

instance (FShow' a, Constructor c) => FShow' (M1 C c a) where
  fshowsPrec' _ n f c@(M1 x) = 
    case fixity of
      Prefix    -> showParen (n > appPrec && not (isNullary x)) 
                    ( showString (conName c) 
                    . if (isNullary x) then id else showChar ' '
                    . showBraces t (fshowsPrec' t appPrec f x))
      Infix _ m -> showParen (n > m) (showBraces t (fshowsPrec' t m f x))
      where fixity = conFixity c
            t = if (conIsRecord c) then Rec else
                  case (conIsTuple c) of
                    True -> Tup
                    False -> case fixity of
                                Prefix    -> Pref
                                Infix _ _ -> Inf (show (conName c))
            showBraces :: Type -> ShowS -> ShowS
            showBraces Rec     p = showChar '{' . p . showChar '}'
            showBraces Tup     p = showChar '(' . p . showChar ')'
            showBraces Pref    p = p
            showBraces (Inf _) p = p
            conIsTuple y = tupleName (conName y) where
              tupleName ('(':',':_) = True
              tupleName _           = False

instance (Selector s, FShow' a) => FShow' (M1 S s a) where
  fshowsPrec' t n f s@(M1 x) | selName s == "" = --showParen (n > appPrec)
                                                 (fshowsPrec' t n f x)
                           | otherwise       =   showString (selName s)
                                               . showString " = "
                                               . fshowsPrec' t 0 f x
  isNullary (M1 x) = isNullary x

instance (FShow' a, Datatype d) => FShow' (M1 D d a) where
  fshowsPrec' t n f d@(M1 x) = showString (datatypeName d ++ ".") . fshowsPrec' t n f x

instance (FShow' a, FShow' b) => FShow' (a :+: b) where
  fshowsPrec' t n f (L1 x) = fshowsPrec' t n f x
  fshowsPrec' t n f (R1 x) = fshowsPrec' t n f x

instance (FShow' a, FShow' b) => FShow' (a :*: b) where
  fshowsPrec' t@Rec     n f (a :*: b) = fshowsPrec' t n     f a . showString ", " . fshowsPrec' t n     f b
  fshowsPrec' t@(Inf s) n f (a :*: b) = fshowsPrec' t n     f a . showString s    . fshowsPrec' t n     f b
  fshowsPrec' t@Tup     n f (a :*: b) = fshowsPrec' t n     f a . showChar   ','  . fshowsPrec' t n     f b
  fshowsPrec' t@Pref    n f (a :*: b) = fshowsPrec' t (n+1) f a . showChar   ' '  . fshowsPrec' t (n+1) f b
  
  -- If we have a product then it is not a nullary constructor
  isNullary _ = False


class FShow a where 
  fshowsPrec :: Int -> FType -> a -> ShowS
  fshows :: FType -> a -> ShowS
  fshows = fshowsPrec 0
  fshow :: FType -> a -> String
  fshow f x = fshows f x ""
#if __GLASGOW_HASKELL__ >= 701
  default fshowsPrec :: (Generic a, FShow' (Rep a))
                     => Int -> FType -> a -> ShowS
  fshowsPrec = fshowsPrecdefault

instance (FShow a) => FShow (Maybe a)

#else

instance (FShow a) => FShow (Maybe a) where
  fshowsPrec = fshowsPrecdefault

#endif

fshowsPrecdefault :: (Generic a, FShow' (Rep a))
                  => Int -> FType -> a -> ShowS
fshowsPrecdefault n f = (fshowsPrec' Pref n f) . from


-- Base types instances
--instance FShow Char   where fshowsPrec = (\_ -> showsPrec)
instance FShow Int    where fshowsPrec = (\i _ -> showsPrec i)
--instance FShow Float  where fshowsPrec = (\_ -> showsPrec)
--instance FShow String where fshowsPrec = (\_ -> showsPrec)
--instance FShow Bool   where fshowsPrec = (\_ -> showsPrec)

