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
    ["newtype Operation a = Operation { fromOperation :: a } deriving Show"
    ,""
    ,"newtype OperationM a = OperationM { fromOperationM :: a } deriving Show"
    ,""
    ,""
    ,"class Call a b c | a b -> c where "
    ,"    call :: a -> b -> c"
    ,""
    ,"instance Call (Operation (a->b)) a (Operation b) where"
    ,"    call (Operation f) a = Operation $ f a"
    ,""
    ,"instance Monad m => Call (Operation (a->b)) (m a) (m (OperationM b)) where"
    ,"    call (Operation f) ma = do"
    ,"        a <- ma"
    ,"        return . OperationM $ f a"
    ,""
    ,"instance Monad m => Call (m (OperationM (a->b))) (m a) (m (OperationM b)) where"
    ,"    call mf ma = do"
    ,"        OperationM f <- mf"
    ,"        a <- ma"
    ,"        return . OperationM $ f a"
    ,""
    ,""
    ,"class Extract a b where "
    ,"    extract :: a -> b"
    ,""
    ,"instance (out~f) => Extract (Operation f) out where"
    ,"    extract (Operation f) = f"
    ,""
    ,"instance (Monad m, m~m2, out~(m f)) => Extract (m (OperationM (m2 f))) out where"
    ,"    extract mop = join $ do"
    ,"        OperationM op <- mop"
    ,"        return op"
    ,""
    ,"instance (Monad m, out~(m f)) => Extract (m (OperationM f)) out where"
    ,"    extract mop = do"
    ,"        OperationM op <- mop"
    ,"        return op"
    ,""
    ,""
    ,"class ToIO a b where"
    ,"    toIO :: a -> b"
    ,""
    ,"instance (out~(IO a)) => ToIO (IO a) out where"
    ,"    toIO = id"
    ,""
    ,"instance (out~(IO a)) => ToIO a out where"
    ,"    toIO = return"
    ]
