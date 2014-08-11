---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE QuasiQuotes   #-}
{-# LANGUAGE TupleSections #-}

module Graph.SampleCodes where

import Text.RawString.QQ

import Flowbox.Prelude



named :: a -> b -> (a, b)
named = (,)

sampleCodes :: [(String,String)]
sampleCodes = [named "simple return" [r|
def main:
    1
|], named "simple infix" [r|
def main:
    1 + 2
|], named "simple assignment 1" [r|
def main:
    x = 0
|], named "simple assignment 2" [r|
def main:
    x = 0
    y = x
|], named "simple assignment 3" [r|
def main:
    x = 0
    {y, _} = x
|], named "following calls" [r|
def foo
def bar

def main:
    1 + 2
    foo
    bar
|], named "following calls 2" [r|
def foo

def main:
    foo
    2
|], named "accessors 1" [r|
def foo

def main:
    foo.bar.baz
    2
|], named "accessors 2" [r|
def main arg:
    arg.bar.baz
    2
|], named "accessors 3" [r|
def x

def main arg:
    x.zooo 43
    -2
|], named "accessors 4" [r|
def x

def main arg:
    x
    x.y
    x.z
|], named "accessors 5" [r|
def main arg:
    x = 4
    x.zooo 43
|], named "accessors 6" [r|
def foo

def main arg:
    foo.bar
|], named "accessors and assignment" [r|
def foo

def main:
    x = foo.bar.baz
    2
|], named "accessors and apps 1" [r|
def foo

def main arg:
    foo.bar arg
    2
|], named "accessors and apps 2" [r|
def foo

def main arg:
    foo.bar arg 2
|], named "complicated inline calls" [r|
def foo

def main arg:
    x = foo.bar(arg, 15, arg, [19..]).baz arg 2
|], named "ranges" [r|
def main arg:
    x = {1, [1..10], [9..]}
|], named "prints" [r|
def print

def main arg arg2:
    print arg
    print arg2
    self.bla "kota" "albo nie"
|], named "constructors 1" [r|
def main arg:
    Main.foo 1 2 3
|], named "constructors 2" [r|
def main arg:
    Foo 1 2 3
|], named "constructors 3" [r|
def main arg:
    Foo arg.boo 1
|], named "constructors 4" [r|
def gap

def main arg:
    Foo arg.boo My gap
|], named "tuples" [r|
def main arg:
    x = 4
    y = {1, x}
|], named "lists" [r|
def main arg:
    x = 4
    y = [1, x]
|]]
