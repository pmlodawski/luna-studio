---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Flowbox Team <contact@flowbox.io>, 2014
-- Proprietary and confidential
-- Unauthorized copying of this file, via any medium is strictly prohibited
---------------------------------------------------------------------------
{-# LANGUAGE QuasiQuotes #-}
module Luna.Interpreter.Session.Helpers where

import Flowbox.Prelude
import Text.RawString.QQ


operation :: String
operation = "default ()"
-- [r|

--newtype Operation a = Operation { fromOperation :: a } deriving Show

--newtype OperationM a = OperationM { fromOperationM :: a } deriving Show


--class Call a b c | a b -> c where
--    call :: a -> b -> c

--instance Call (Operation (a->b)) a (Operation b) where
--    call (Operation f) a = Operation $ f a

--instance Monad m => Call (Operation (a->b)) (m a) (m (OperationM b)) where
--    call (Operation f) ma = do
--        a <- ma
--        return . OperationM $ f a

--instance Monad m => Call (m (OperationM (a->b))) (m a) (m (OperationM b)) where
--    call mf ma = do
--        OperationM f <- mf
--        a <- ma
--        return . OperationM $ f a


--class Extract a b where
--    extract :: a -> b

--instance (out~f) => Extract (Operation f) out where
--    extract (Operation f) = f

--instance (Monad m, m~m2, out~(m f)) => Extract (m (OperationM (m2 f))) out where
--    extract mop = join $ do
--        OperationM op <- mop
--        return op

--instance (Monad m, out~(m f)) => Extract (m (OperationM f)) out where
--    extract mop = do
--        OperationM op <- mop
--        return op


--class ToIO a b where
--    toIO :: a -> b

--instance (out~(IO a)) => ToIO (IO a) out where
--    toIO = id

--instance (out~(IO a)) => ToIO a out where
--    toIO = return
-- |]


-- {-# LANGUAGE EmptyDataDecls            #-}
-- {-# LANGUAGE FlexibleInstances         #-}
-- {-# LANGUAGE FunctionalDependencies    #-}
-- {-# LANGUAGE GADTs                     #-}
-- {-# LANGUAGE MultiParamTypeClasses     #-}
-- {-# LANGUAGE ScopedTypeVariables       #-}
-- {-# LANGUAGE OverlappingInstances      #-}
-- {-# LANGUAGE UndecidableInstances      #-}
--
-- import Prelude
-- import qualified Data.Hash
-- import  Data.Word
hash :: String
hash = "hash _ = Nothing"
-- [r|
--class Hash a where
--    hash :: a -> Maybe Word64


--class Hash' flag a where
--    hash' :: flag -> a -> Maybe Word64

--instance (HashablePred a flag, Hash' flag a) => Hash a where
--    hash = hash' (undefined::flag)


---- overlapping instances are used only for HashablePred
--class HashablePred a flag | a->flag where {}

--                                  -- Used only if the other
--                                  -- instances don't apply
--instance TypeCast flag HFalse => HashablePred a flag

--instance HashablePred Int    HTrue   -- These instances should be
--instance HashablePred Bool   HTrue   -- the same as Hashable's
--instance HashablePred Char   HTrue   -- ...
--instance HashablePred [Char] HTrue   -- ...
--instance HashablePred a flag => HashablePred [a] flag
----  ...etc...


--data HTrue    -- Just two
--data HFalse   -- distinct types

--instance Data.Hash.Hashable a => Hash' HTrue a where
--   hash' _ x = Just $ Data.Hash.asWord64 $ Data.Hash.hash x
--instance Hash' HFalse a where
--   hash' _ x = Nothing


-- -- see http://okmij.org/ftp/Haskell/typecast.html
--class TypeCast   a b   | a -> b, b->a   where typeCast   :: a -> b
--class TypeCast'  t a b | t a -> b, t b -> a where typeCast'  :: t->a->b
--class TypeCast'' t a b | t a -> b, t b -> a where typeCast'' :: t->a->b
--instance TypeCast'  () a b => TypeCast a b where typeCast x = typeCast' () x
--instance TypeCast'' t a b => TypeCast' t a b where typeCast' = typeCast''
--instance TypeCast'' () a a where typeCast'' _ x  = x
-- |]
