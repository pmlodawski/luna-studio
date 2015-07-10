{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TypeFamilies    #-}

module Generator.Cython.Data where

import GHC.Generics           (Generic)
import Data.Binary
import Generator.Cython.CyGen (generateBindings, collectBindings)
import Generator.Cython.Utils (moduleName)

---------------------------------------------------------------------

data Foo = Foo Int String deriving (Show, Read, Eq, Ord, Generic)
generateBindings ''Foo "test2"

instance Binary Foo

---------------------------------------------------------------------

data Bar = Bar Int Foo | Baz String deriving (Show, Read, Eq, Ord, Generic)
generateBindings ''Bar "test2"

instance Binary Bar

---------------------------------------------------------------------

data Bum a b = Bum a b deriving (Show, Read, Eq, Ord, Generic)
generateBindings ''Bum "test2"

instance (Binary a, Binary b) => Binary (Bum a b)

---------------------------------------------------------------------

collectBindings $(moduleName) "test2"
