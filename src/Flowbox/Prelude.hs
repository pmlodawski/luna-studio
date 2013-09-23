---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE NoMonomorphismRestriction #-}

module Flowbox.Prelude(
    module Flowbox.Prelude,
    module Prelude
) where

import           Prelude                hiding (print, putStr, putStrLn, mapM, mapM_)
import qualified Prelude                as Prelude
import           Control.Monad.IO.Class   (liftIO, MonadIO)
import           Data.Typeable            
import qualified Data.Traversable       as Traversable
import           Data.Traversable         (Traversable)

print :: (MonadIO m, Show s) => s -> m ()
print    = liftIO . Prelude.print

putStr :: MonadIO m => String -> m ()
putStr   = liftIO . Prelude.putStr

putStrLn :: MonadIO m => String -> m ()
putStrLn = liftIO . Prelude.putStrLn


instance (Typeable a) => Show (IO a) where
    show e = '(' : (show . typeOf) e ++ ")"

instance (Typeable a, Typeable b) => Show (a -> b) where
    show e = '(' : (show . typeOf) e ++ ")"

-- f .: g = \x y->f (g x y)
-- f .: g = (f .) . g
-- (.:) f = ((f .) .)
-- (.:) = (.) (.) (.)
(.:)  :: (x -> y) -> (a -> b -> x) -> (a -> b -> y)
(.:)   = (.) . (.)

(.:.) :: (x -> y) -> (a -> b -> c -> x) -> (a -> b -> c -> y)
(.:.)  = (.) . (.) . (.)

(.::) :: (x -> y) -> (a -> b -> c -> d -> x) -> (a -> b -> c -> d -> y)
(.::)  = (.) . (.) . (.) . (.)

mapM :: (Monad m, Traversable t) => (a -> m b) -> t a -> m (t b)
mapM = Traversable.mapM

mapM_ :: (Monad m, Traversable t) => (a -> m b) -> t a -> m ()
mapM_ f as = do 
    _ <- mapM f as
    return ()