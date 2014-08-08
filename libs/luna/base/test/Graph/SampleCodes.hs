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
sampleCodes = [named "test1" [r|
def main:
    1
|], named "test2" [r|
def main:
    1 + 2
|], named "test3" [r|
def main:
    x = 0
|], named "test4" [r|
def foo
def bar

def main:
    1 + 2
    foo
    bar
|], named "test5" [r|
def foo

def main:
    foo
    2
|], named "test6" [r|
def foo

def main:
    foo.bar.baz
    2
|], named "test7" [r|
def main arg:
    arg.bar.baz
    2
|], named "test8" [r|
def foo

def main:
    x = foo.bar.baz
    2
|], named "test9" [r|
def foo

def main arg:
    foo.bar arg
    2
|], named "test10" [r|
def foo

def main arg:
    foo.bar arg 2
|], named "test11" [r|
def foo

def main arg:
    x = foo.bar(arg, 15, arg, [19..]).baz arg 2
|], named "test12" [r|
def x

def main arg:
    x.zooo 43
    -2
|], named "test13" [r|
def x

def main arg:
    x
    x.y
    x.z
|], named "test14" [r|
def main arg:
    x = 4
    x.zooo 43
|], named "test15" [r|
def foo

def main arg:
    foo.bar
|], named "test16" [r|
def main arg:
    x = {1, [1..10], [9..]}
|], named "test17" [r|
def print

def main arg arg2:
    print arg
    print arg2
    self.bla "kota" "albo nie"
|], named "test18" [r|
def main arg:
    Main.foo 1 2 3
|], named "test19" [r|
def main arg:
    Foo 1 2 3
|], named "test20" [r|
def main arg:
    Foo arg.boo 1
|], named "test21" [r|
def gap

def main arg:
    Foo arg.boo My gap
|]]
-- |], named "test17" [r|
-- def main arg:
--     x = 4
--     y = {1, x}
-- |]]

