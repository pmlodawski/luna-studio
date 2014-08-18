{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-} 
 
module Main where
 
class Pipe a b c | a b -> c where
    pipe :: a -> b -> c

testme :: (Monad m, Pipe (m Int) a out) => a -> out 
testme a = pipe (return (0::Int)) a


--testme :: forall m a out. (Monad m, Pipe (m Int) a out) => a -> out
--testme a = pipe (return 0 :: m Int) a

--testme :: forall m a out. (Monad m, Pipe (m Int) a out) => a -> out 
--testme a = pipe (return 0::m Int) a

--ERROR
--    Could not deduce (Pipe (m0 Int) a out)
--      arising from the ambiguity check for ‘testme’
--    from the context (Monad m, Pipe (m Int) a out)
--      bound by the type signature for
--                 testme :: (Monad m, Pipe (m Int) a out) => a -> out
--      at Tmp.hs:11:11-51
--    The type variable ‘m0’ is ambiguous
--    In the ambiguity check for:
--      forall a out (m :: * -> *).
--      (Monad m, Pipe (m Int) a out) =>
--      a -> out
--    To defer the ambiguity check to use sites, enable AllowAmbiguousTypes
--    In the type signature for ‘testme’:
--      testme :: (Monad m, Pipe (m Int) a out) => a -> out



--{-# LANGUAGE FunctionalDependencies #-} -- all the extensions 
--{-# LANGUAGE ScopedTypeVariables #-} 
--{-# LANGUAGE AllowAmbiguousTypes #-} 


--instance Pipe (IO a) Int Int where 
--    pipe _ x = x -- You can do this to successfully define "testme" 

