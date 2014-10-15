---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Luna.Sample.Program where

import Text.RawString.QQ

import Flowbox.Prelude



type Name   = String
type Code   = String
type Output = String


data Program = Program { _name   :: Name
                       , _code   :: Code
                       , _output :: Output
                       } deriving (Show)

makeLenses ''Program


programs :: [Program]
programs = [

    Program "empty" [r|

def main:
    1
|] "",

    Program "hello world" [r|

def putStrLn msg:
    ```autoLift1 putStrLn #{msg}```

def main:
    putStrLn "Hello World!"

|] [r|Hello World!
|],

    Program "Vector 1 2 3" [r|

class Vector a:
    x,y,z :: a
    def test a b:
        {a,b}

def print msg:
    ```autoLift1 print #{msg}```

def main:
    v = Vector 1 2 3
    print $ v
|] [r|Vector 1 2 3
|],

    Program "Vector, if, Int.+ and Int.>" [r|

class Vector a:
    x,y,z :: a
    def test a b:
        {a,b}

def print msg:
    ```autoLift1 print #{msg}```

def Int.+ a:
    ```liftF2 (+) #{self} #{a}```

def Int.> a:
    ```liftF2 (>) #{self} #{a}```

def Int.inc:
    self + 1

def main:
    print $ if 1 > 2: 5
            else: 6
    print $ 1 > 2
    v = Vector 1 2 3
    print $ v
|] [r|6
False
Vector 1 2 3
|]]
