---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Flowbox.Prelude(
        module Control.Applicative,
    module Control.Lens,
    module Data.Monoid,
    module Flowbox.Prelude,
    module Prelude
) where

import           Control.Applicative
import qualified Control.Exception      as Exception
import           Control.Lens
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Monoid            (Monoid, mappend, mempty)
import           Data.Traversable       (Traversable)
import qualified Data.Traversable       as Traversable
import           Data.Typeable
import           Prelude                hiding (mapM, mapM_, print, putStr, putStrLn, (++))
import qualified Prelude                as Prelude

(++) :: Monoid a => a -> a -> a
(++) = mappend

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

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _        = False

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _         = False


fromJust :: Maybe a -> IO a
fromJust Nothing  = Exception.throwIO $ Exception.ErrorCall ("Maybe.fromJust: Nothing" :: String)
fromJust (Just x) = return x


either2io :: Either String b -> IO b
either2io f = case f of
        Right r -> return r
        Left  e -> fail e
