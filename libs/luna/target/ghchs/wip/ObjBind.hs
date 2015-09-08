{-# LANGUAGE NoMonomorphismRestriction #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-} 
{-# LANGUAGE DysfunctionalDependencies #-} 

{-# LANGUAGE OverlappingInstances #-} 
{-# LANGUAGE UndecidableInstances #-} 



import Control.Monad.State
import Control.Monad.Reader
import Control.Applicative
import Data.Functor.Identity (Identity)

import GHC.TypeLits        (Symbol)

data Proxy a = Proxy

print' = liftIO . print



data X1 = X1 deriving Show
data X2 = X2 deriving Show


class Method (name :: Symbol) base func | base -> func where
    method :: Proxy name -> base -> func

--test :: [Int]
test = return 1

instance Monad m => Method "test" X1 (m Int) where
    method _ _ = test


instance Method "test" (m Int) Int where
    method _ _ = 5

--instance Method "test" X1 [Int] where
--    method _ _ = test


main = do
    print $ method (Proxy::Proxy "test") $ (method (Proxy::Proxy "test") X1 :: [Int])
    --print $ method (Proxy::Proxy "test") $ (method (Proxy::Proxy "test"))