---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Flowbox Team <contact@flowbox.io>, 2014
-- Proprietary and confidential
-- Unauthorized copying of this file, via any medium is strictly prohibited
---------------------------------------------------------------------------
module Flowbox.Interpreter.Session.Helpers where

import Flowbox.Prelude


helpers :: String
helpers = unlines
    [ "newtype Operation a = Operation { fromOperation :: a } deriving Show"
    , ""
    , "class Call a b c | a b -> c where "
    , "    call :: a -> b -> c"
    , ""
    , "instance Call (Operation (a->b)) a (Operation b) where"
    , "    call (Operation f) a = Operation $ f a"
    , ""
    , "instance Monad m => Call (Operation (a->b)) (m a) (m (Operation b)) where"
    , "    call (Operation f) ma = do"
    , "        a <- ma"
    , "        return . Operation $ f a"
    , ""
    , "instance Monad m => Call (m (Operation (a->b))) (m a) (m (Operation b)) where"
    , "    call mf ma = do"
    , "        Operation f <- mf"
    , "        a <- ma"
    , "        return . Operation $ f a"
    , ""
    , "class Extract a b | a -> b where "
    , "    extract :: a -> b"
    , ""
    , "instance Extract (Operation f) f where"
    , "    extract (Operation f) = f"
    , ""
    , "instance Monad m => Extract (m (Operation f)) (m f) where"
    , "    extract mop = do"
    , "        Operation op <- mop"
    , "        return op"
    ]
