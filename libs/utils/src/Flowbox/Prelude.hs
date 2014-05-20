---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Flowbox.Prelude(
    module Flowbox.Prelude,
    module Prelude,
    module X,
    void
) where

import           Control.Applicative    as X
import           Control.Lens           as X
import           Control.Monad          (void)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Trans    (lift)
import           Data.Default           as X
import           Data.Foldable          (forM_)
import           Data.Monoid            as X (Monoid, mappend, mempty)
import qualified Data.Traversable       as Traversable

import           Flowbox.Debug.Debug as X
import           Prelude             hiding (mapM, mapM_, print, putStr, putStrLn, (++), (.))
import qualified Prelude



(++) :: Monoid a => a -> a -> a
(++) = mappend

print :: (MonadIO m, Show s) => s -> m ()
print    = liftIO . Prelude.print

putStr :: MonadIO m => String -> m ()
putStr   = liftIO . Prelude.putStr

putStrLn :: MonadIO m => String -> m ()
putStrLn = liftIO . Prelude.putStrLn


--instance (Typeable a) => Show (IO a) where
--    show e = '(' : (show . typeOf) e ++ ")"

--instance (Typeable a, Typeable b) => Show (a -> b) where
--    show e = '(' : (show . typeOf) e ++ ")"

-- f .: g = \x y->f (g x y)
-- f .: g = (f .) . g
-- (.:) f = ((f .) .)
-- (.:) = (.) (.) (.)
infixr 9 .
(.) :: (Functor f) => (a -> b) -> f a -> f b
(.) = fmap

(.:)  :: (x -> y) -> (a -> b -> x) -> a -> b -> y
(.:)   = (.) . (.)

(.:.) :: (x -> y) -> (a -> b -> c -> x) -> a -> b -> c -> y
(.:.)  = (.) . (.) . (.)

(.::) :: (x -> y) -> (a -> b -> c -> d -> x) -> a -> b -> c -> d -> y
(.::)  = (.) . (.) . (.) . (.)

mapM :: (Monad m, Traversable t) => (a -> m b) -> t a -> m (t b)
mapM = Traversable.mapM

mapM_ :: (Monad m, Traversable t) => (a -> m b) -> t a -> m ()
mapM_ f as = do
    _ <- mapM f as
    return ()

mkList :: a -> [a]
mkList a = [a]

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _        = False

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _         = False


fromJust :: Monad m => Maybe a -> m a
fromJust Nothing  = fail "Maybe.fromJust: Nothing"
fromJust (Just x) = return x


whenLeft :: (Monad m) => Either a b -> (a -> m ()) -> m ()
whenLeft e f = case e of
    Left  v -> f v
    Right _ -> return ()


whenLeft' :: (Monad m) => Either a b -> m () -> m ()
whenLeft' e f = whenLeft e (const f)


whenRight :: (Monad m) => Either a b -> (b -> m ()) -> m ()
whenRight e f = case e of
    Left  _ -> return ()
    Right v -> f v


whenRight' :: (Monad m) => Either a b -> m () -> m ()
whenRight' e f = whenRight e (\_ -> f)

-- trenary operator
data Cond a = a :? a

infixl 0 ?
infixl 1 :?

(?) :: Bool -> Cond a -> a
True  ? (x :? _) = x
False ? (_ :? y) = y
-- / trenaru operator


($>) :: (Functor f) => a -> f b -> f b
($>) =  fmap . flip const


withJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
withJust = forM_

lift2 = lift . lift


ifM :: (Monad m) => m Bool -> m a -> m a -> m a
ifM predicate a b = do bool <- predicate
                       if bool then a else b
