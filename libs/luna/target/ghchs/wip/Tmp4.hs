{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DysfunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}


newtype Req req m a = Req (m a)
newtype Value m a = Value { fromValue :: m a }
newtype Ctx m a = Ctx { fromCtx :: m a }

class AppMonadCtx2 a b | a -> b where
    appMonadCtx2 :: a -> b

instance AppMonadCtx2 (Ctx m a) (Req req m a) where
    appMonadCtx2 = Req . fromCtx

instance AppMonadCtx2 (Req req m a) (Req req m a) where
    appMonadCtx2 = id

instance AppMonadCtx2 (Value m a) (Req req m a) where
    appMonadCtx2 = undefined


needsReq :: Req req m a -> b -> a
needsReq = undefined

tst a = needsReq (appMonadCtx2 a)

