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

module FlowboxM.Utils.Generics.Show (
  -- * Generic show class
    LShow(..)

  -- * Default definition
  , lshowsPrecdefault

  ) where


import           Generics.Deriving.Base        
import           Generics.Deriving.Instances   ()

--import           Flowbox.Prelude          

--------------------------------------------------------------------------------
-- Generic show
--------------------------------------------------------------------------------

appPrec :: Int
appPrec = 2

data Type = Rec | Tup | Pref | Inf String

class LShow' f where
  lshowsPrec' :: Type -> Int -> f a -> ShowS
  isNullary   :: f a -> Bool
  isNullary = error "generic show (isNullary): unnecessary case"

instance LShow' U1 where
  lshowsPrec' _ _ U1 = id
  isNullary _ = True

instance (LShow c) => LShow' (K1 i c) where
  lshowsPrec' _ n (K1 a) = lshowsPrec n a
  isNullary _ = False

-- No instances for P or Rec because lshow is only applicable to types of kind *

instance (LShow' a, Constructor c) => LShow' (M1 C c a) where
  lshowsPrec' _ n c@(M1 x) = 
    case fixity of
      Prefix    -> showParen (n > appPrec && not (isNullary x)) 
                    ( showString (conName c) 
                    . if (isNullary x) then id else showChar ' '
                    . showBraces t (lshowsPrec' t appPrec x))
      Infix _ m -> showParen (n > m) (showBraces t (lshowsPrec' t m x))
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

instance (Selector s, LShow' a) => LShow' (M1 S s a) where
  lshowsPrec' t n s@(M1 x) | selName s == "" = --showParen (n > appPrec)
                                                 (lshowsPrec' t n x)
                           | otherwise       =   showString (selName s)
                                               . showString " = "
                                               . lshowsPrec' t 0 x
  isNullary (M1 x) = isNullary x

instance (LShow' a, Datatype d) => LShow' (M1 D d a) where
  lshowsPrec' t n d@(M1 x) = showString (datatypeName d ++ ".") . lshowsPrec' t n x

instance (LShow' a, LShow' b) => LShow' (a :+: b) where
  lshowsPrec' t n (L1 x) = lshowsPrec' t n x
  lshowsPrec' t n (R1 x) = lshowsPrec' t n x

instance (LShow' a, LShow' b) => LShow' (a :*: b) where
  lshowsPrec' t@Rec     n (a :*: b) =
    lshowsPrec' t n     a . showString ", " . lshowsPrec' t n     b
  lshowsPrec' t@(Inf s) n (a :*: b) =
    lshowsPrec' t n     a . showString s    . lshowsPrec' t n     b
  lshowsPrec' t@Tup     n (a :*: b) =
    lshowsPrec' t n     a . showChar ','    . lshowsPrec' t n     b
  lshowsPrec' t@Pref    n (a :*: b) =
    lshowsPrec' t (n+1) a . showChar ' '    . lshowsPrec' t (n+1) b
  
  -- If we have a product then it is not a nullary constructor
  isNullary _ = False


class LShow a where 
  lshowsPrec :: Int -> a -> ShowS
  lshows :: a -> ShowS
  lshows = lshowsPrec 0
  lshow :: a -> String
  lshow x = lshows x ""
#if __GLASGOW_HASKELL__ >= 701
  default lshowsPrec :: (Generic a, LShow' (Rep a))
                     => Int -> a -> ShowS
  lshowsPrec = lshowsPrecdefault

instance (LShow a) => LShow (Maybe a)

#else

instance (LShow a) => LShow (Maybe a) where
  lshowsPrec = lshowsPrecdefault

#endif

lshowsPrecdefault :: (Generic a, LShow' (Rep a))
                  => Int -> a -> ShowS
lshowsPrecdefault n = lshowsPrec' Pref n . from


-- Base types instances
instance LShow Char   where lshowsPrec = showsPrec
instance LShow Int    where lshowsPrec = showsPrec
instance LShow Float  where lshowsPrec = showsPrec
instance LShow String where lshowsPrec = showsPrec
instance LShow Bool   where lshowsPrec = showsPrec

intersperse :: a -> [a] -> [a]
intersperse _ []    = []
intersperse _ [h]   = [h]
intersperse x (h:t) = h : x : (intersperse x t)

instance (LShow a) => LShow [a] where
  lshowsPrec _ l =   showChar '['
                   . foldr (.) id
                      (intersperse (showChar ',') (map (lshowsPrec 0) l))
                   . showChar ']'

