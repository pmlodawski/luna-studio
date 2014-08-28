{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}

import Unsafe.Coerce

newtype Req req m a = Req (m a)
newtype Value m a = Value { fromValue :: m a }
newtype Ctx m a = Ctx { fromCtx :: m a }


data ReqV
-- placeholder for an actual polymorphic variable
-- same trick as with the C and c in
-- http://hackage.haskell.org/package/HList-0.3.4.1/docs/src/Data-HList-Data.html

class AppMonadCtx2 a where
    type family MkB a
    appMonadCtx2 :: a -> MkB a

instance AppMonadCtx2 (Ctx m a) where
    type MkB (Ctx m a) = Req ReqV m a
    appMonadCtx2 = Req . fromCtx

instance AppMonadCtx2 (Req req m a)  where
    type MkB (Req req m a) = (Req req m a)
    appMonadCtx2 = id

instance AppMonadCtx2 (Value m a)  where
    type MkB (Value m a) = (Req ReqV m a)
    appMonadCtx2 = undefined

--appMonadCtxReal :: AppMonadCtx2 a => a -> (forall req m a1. (Req req m a1) ~ MkB a => Req req m a1)
appMonadCtxReal :: AppMonadCtx2 a => a -> (forall req m a1. (Req ReqV m a1) ~ MkB a => Req req m a1)
appMonadCtxReal = castV . appMonadCtx2
  where
    castV :: Req req x y -> Req req' x y
    castV = unsafeCoerce -- or maybe Data.Coerce.coerce


needsReq :: Req req m a -> b -> a
needsReq = undefined

tst a = needsReq (appMonadCtxReal a)