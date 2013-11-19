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

module FlowboxM.Utils.Generics.Repr (
  -- * Generic show class
    LRepr(..)

  -- * Default definition
  , lreprsPrecdefault

  ) where


import           Generics.Deriving.Base        
import           Generics.Deriving.Instances   ()

import           Debug.Trace                   

--import           Flowbox.Prelude          

--------------------------------------------------------------------------------
-- Generic value representation
--------------------------------------------------------------------------------

appPrec :: Int
appPrec = 2

data Type = Rec | Tup | Pref | Inf String deriving(Show)

isTup t = case t of
    Tup -> True
    _   -> False

class LRepr' f where
  lreprsPrec' :: Type -> Int -> f a -> ShowS
  isNullary   :: f a -> Bool
  isNullary = error "generic show (isNullary): unnecessary case"

instance LRepr' U1 where
  lreprsPrec' _ _ U1 = id
  isNullary _ = True

instance (LRepr c) => LRepr' (K1 i c) where
  lreprsPrec' _ n (K1 a) = lreprsPrec n a
  isNullary _ = False

-- No instances for P or Rec because lrepr is only applicable to types of kind *

instance (LRepr' a, Constructor c) => LRepr' (M1 C c a) where
  lreprsPrec' _ n c@(M1 x) = 
    case fixity of
      Prefix    -> showParen (n > appPrec && not (isNullary x)) 
                    ( showCon c
                    . (if (isNullary x || isTup t) then id else showChar ' ')
                    . showBraces t (lreprsPrec' t appPrec x))
      Infix _ m -> showParen (n > m) (showBraces t (lreprsPrec' t m x))
      where fixity = conFixity c
            t = if (conIsRecord c) then Rec else
                  case (conIsTuple c) of
                    True  -> Tup
                    False -> case fixity of
                                Prefix    -> Pref
                                Infix _ _ -> Inf (show (conName c))

            
            showCon c = case t of
                Tup   -> showString ""
                _     -> showString (conName c) 

            showBraces :: Type -> ShowS -> ShowS
            showBraces t p = case t of Rec     -> showChar '{' . p . showChar '}'
                                       Tup     -> showChar '{' . p . showChar '}'
                                       Pref    -> p
                                       (Inf _) -> p
            conIsTuple y = tupleName (conName y) where
              tupleName ('(':',':_) = True
              tupleName _           = False

instance (Selector s, LRepr' a) => LRepr' (M1 S s a) where
  lreprsPrec' t n s@(M1 x) | selName s == "" = --showParen (n > appPrec)
                                                 (lreprsPrec' t n x)
                           | otherwise       =   showString (selName s)
                                               . showString " = "
                                               . lreprsPrec' t 0 x
  isNullary (M1 x) = isNullary x

instance (LRepr' a, Datatype d) => LRepr' (M1 D d a) where
  lreprsPrec' t n d@(M1 x) = lreprsPrec' t n x

instance (LRepr' a, LRepr' b) => LRepr' (a :+: b) where
  lreprsPrec' t n (L1 x) = lreprsPrec' t n x
  lreprsPrec' t n (R1 x) = lreprsPrec' t n x

instance (LRepr' a, LRepr' b) => LRepr' (a :*: b) where
  lreprsPrec' t@Rec     n (a :*: b) =
    lreprsPrec' t n     a . showString ", " . lreprsPrec' t n     b
  lreprsPrec' t@(Inf s) n (a :*: b) =
    lreprsPrec' t n     a . showString s    . lreprsPrec' t n     b
  lreprsPrec' t@Tup     n (a :*: b) =
    lreprsPrec' t n     a . showChar ','    . lreprsPrec' t n     b
  lreprsPrec' t@Pref    n (a :*: b) =
    lreprsPrec' t (n+1) a . showChar ' '    . lreprsPrec' t (n+1) b
  
  -- If we have a product then it is not a nullary constructor
  isNullary _ = False


class LRepr a where 
  lreprsPrec :: Int -> a -> ShowS
  lreprs :: a -> ShowS
  lreprs = lreprsPrec 0
  lrepr :: a -> String
  lrepr x = lreprs x ""
#if __GLASGOW_HASKELL__ >= 701
  default lreprsPrec :: (Generic a, LRepr' (Rep a))
                     => Int -> a -> ShowS
  lreprsPrec = lreprsPrecdefault

instance (LRepr a) => LRepr (Maybe a)

#else

instance (LRepr a) => LRepr (Maybe a) where
  lreprsPrec = lreprsPrecdefault

#endif

lreprsPrecdefault :: (Generic a, LRepr' (Rep a))
                  => Int -> a -> ShowS
lreprsPrecdefault n = lreprsPrec' Pref n . from


-- Base types instances
instance LRepr Char   where lreprsPrec = showsPrec
instance LRepr Int    where lreprsPrec = showsPrec
instance LRepr Float  where lreprsPrec = showsPrec
instance LRepr Bool   where lreprsPrec = showsPrec
instance LRepr String where lrepr      = id

intersperse :: a -> [a] -> [a]
intersperse _ []    = []
intersperse _ [h]   = [h]
intersperse x (h:t) = h : x : (intersperse x t)

instance (LRepr a) => LRepr [a] where
  lreprsPrec _ l =   showChar '['
                   . foldr (.) id
                      (intersperse (showChar ',') (map (lreprsPrec 0) l))
                   . showChar ']'

