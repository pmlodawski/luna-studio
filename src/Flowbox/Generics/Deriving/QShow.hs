---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
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

module Flowbox.Generics.Deriving.QShow (
  -- * Generic show class
    QShow(..)

  -- * Default definition
  , qshowsPrecdefault

  ) where


import           Generics.Deriving.Base        
import           Generics.Deriving.Instances   ()

import           Flowbox.Prelude             hiding (from)

--------------------------------------------------------------------------------
-- Generic show
--------------------------------------------------------------------------------

appPrec :: Int
appPrec = 2

data Type = Rec | Tup | Pref | Inf String

class QShow' f where
  qshowsPrec' :: Type -> Int -> f a -> ShowS
  isNullary   :: f a -> Bool
  isNullary = error "generic show (isNullary): unnecessary case"

instance QShow' U1 where
  qshowsPrec' _ _ U1 = id
  isNullary _ = True

instance (QShow c) => QShow' (K1 i c) where
  qshowsPrec' _ n (K1 a) = qshowsPrec n a
  isNullary _ = False

-- No instances for P or Rec because qshow is only applicable to types of kind *

instance (QShow' a, Constructor c) => QShow' (M1 C c a) where
  qshowsPrec' _ n c@(M1 x) = 
    case fixity of
      Prefix    -> showParen (n > appPrec && not (isNullary x)) 
                    ( showString (conName c) 
                    . if (isNullary x) then id else showChar ' '
                    . showBraces t (qshowsPrec' t appPrec x))
      Infix _ m -> showParen (n > m) (showBraces t (qshowsPrec' t m x))
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

instance (Selector s, QShow' a) => QShow' (M1 S s a) where
  qshowsPrec' t n s@(M1 x) | selName s == "" = --showParen (n > appPrec)
                                                 (qshowsPrec' t n x)
                           | otherwise       =   showString (selName s)
                                               . showString " = "
                                               . qshowsPrec' t 0 x
  isNullary (M1 x) = isNullary x

instance (QShow' a, Datatype d) => QShow' (M1 D d a) where
  qshowsPrec' t n d@(M1 x) = showString (datatypeName d ++ ".") . qshowsPrec' t n x

instance (QShow' a, QShow' b) => QShow' (a :+: b) where
  qshowsPrec' t n (L1 x) = qshowsPrec' t n x
  qshowsPrec' t n (R1 x) = qshowsPrec' t n x

instance (QShow' a, QShow' b) => QShow' (a :*: b) where
  qshowsPrec' t@Rec     n (a :*: b) =
    qshowsPrec' t n     a . showString ", " . qshowsPrec' t n     b
  qshowsPrec' t@(Inf s) n (a :*: b) =
    qshowsPrec' t n     a . showString s    . qshowsPrec' t n     b
  qshowsPrec' t@Tup     n (a :*: b) =
    qshowsPrec' t n     a . showChar ','    . qshowsPrec' t n     b
  qshowsPrec' t@Pref    n (a :*: b) =
    qshowsPrec' t (n+1) a . showChar ' '    . qshowsPrec' t (n+1) b
  
  -- If we have a product then it is not a nullary constructor
  isNullary _ = False


class QShow a where 
  qshowsPrec :: Int -> a -> ShowS
  qshows :: a -> ShowS
  qshows = qshowsPrec 0
  qshow :: a -> String
  qshow x = qshows x ""
#if __GLASGOW_HASKELL__ >= 701
  default qshowsPrec :: (Generic a, QShow' (Rep a))
                     => Int -> a -> ShowS
  qshowsPrec = qshowsPrecdefault

instance (QShow a) => QShow (Maybe a)

#else

instance (QShow a) => QShow (Maybe a) where
  qshowsPrec = qshowsPrecdefault

#endif

qshowsPrecdefault :: (Generic a, QShow' (Rep a))
                  => Int -> a -> ShowS
qshowsPrecdefault n = qshowsPrec' Pref n . from


-- Base types instances
instance QShow Char   where qshowsPrec = showsPrec
instance QShow Int    where qshowsPrec = showsPrec
instance QShow Float  where qshowsPrec = showsPrec
instance QShow String where qshowsPrec = showsPrec
instance QShow Bool   where qshowsPrec = showsPrec

intersperse :: a -> [a] -> [a]
intersperse _ []    = []
intersperse _ [h]   = [h]
intersperse x (h:t) = h : x : (intersperse x t)

instance (QShow a) => QShow [a] where
  qshowsPrec _ l =   showChar '['
                   . foldr (.) id
                      (intersperse (showChar ',') (map (qshowsPrec 0) l))
                   . showChar ']'

