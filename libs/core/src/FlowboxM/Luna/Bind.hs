{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PolyKinds                 #-}

module FlowboxM.Luna.Bind where

import FlowboxM.Luna.Data

-- redefinition of do notation

pureIO :: a -> IO a
pureIO = Prelude.return

failIO :: String -> IO a
failIO = Prelude.fail


(>>=) = bind
(>>)  = bind_
fail  = failIO
return a = a

class Bind m1 m2 where
    bind :: m1 a -> (Pure a -> m2 b) -> IO b


instance (GetIO a, GetIO b) => Bind a b where
    bind a b = do
        va <- getIO a
        getIO $ b (Pure va)

bind_ a b = bind a (\_ -> b)
