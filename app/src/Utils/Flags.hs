{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverlappingInstances #-}

module Flags where

import Data.Bits



newtype Flags = Flags { fromFlags :: Int } deriving (Eq, Bits, Show)


class ToFlags a where
    toFlags :: a -> Flags

instance Enum a => ToFlags a where
    toFlags = Flags . bit . fromEnum

instance ToFlags Flags where
    toFlags = id

data Mods = Control
          | Alt
          | Shift
          | Meta
          deriving (Show, Enum)

control = toFlags Control :: Flags
alt     = toFlags Alt     :: Flags
shift   = toFlags Shift
meta    = toFlags Meta

class FlagJoin a b where
    flagJoin :: a -> b -> Flags

instance FlagJoin Flags Flags where
    flagJoin = (.|.)

instance (ToFlags a, ToFlags b) => FlagJoin a b where
    flagJoin a b = toFlags a .|. toFlags b


(<|>) = flagJoin


data Foo = A
         | B
         | C
         deriving (Show, Enum)

main = do
    print "hello"
    let b1 = zeroBits :: Int
        b2 = b1 .|. bit 3
    print b2

    print $ control <|> alt


test a
    | a == control <|> alt  = "a"
    | a == control <|> meta = "a"
    | a == control          = "a"


test2 a
    | a == A <|> B  = "a”
