---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}

module Prologue.Class (
    module Prologue.Class,
    module Prelude,
    module X
) where

import qualified Data.Traversable                   as Traversable

import           Data.Binary.Instances.Missing      ()
import           Data.Default.Instances.Missing     ()
import           Data.Foldable                      (forM_)
import           Data.List                          (intersperse)
import           Prelude                            hiding (mapM, mapM_, print, putStr, putStrLn, (++), (.))
import qualified Prelude
import           Text.Show.Pretty                   (ppShow)

import           Data.Function                      as X (on)
import           Data.Maybe                         as X (mapMaybe)
import           Data.Default                       as X
import           Control.Lens                       as X hiding (Wrapped)
import           Data.String.Class                  as X (IsString (fromString), ToString (toString))
import           Control.Applicative                as X
import           Control.Conditional                as X (ifM, unless, unlessM, when, whenM)
import           Control.Monad                      as X (MonadPlus, mplus, mzero, void)
import           Control.Monad.IO.Class             as X (MonadIO, liftIO)
import           Control.Monad.Trans                as X (MonadTrans, lift)
import           Data.Wrapper                       as X (Unwrap (unwrap), UnwrapT (unwrapT), Wrap (wrap), WrapT (wrapT), Wrapper, WrapperT, rewrap, wrapped, Wrapped)
import           Data.Foldable                      as X (Foldable, traverse_)
import           Data.Monoid                        as X (Monoid, mappend, mconcat, mempty, (<>))
import           Data.String.Repr                   as X (StrRepr, strRepr)
import           Data.Text.Class                    as X (FromText (fromText), IsText, ToText (toText))
import           Data.Text.Lazy                     as X (Text)
import           Data.Typeable                      as X (Typeable)
import           GHC.Exts                           as X (IsList, Item, fromList, fromListN, toList)
import           GHC.Generics                       as X (Generic)
import           Data.Content                       as X (Content, Content', content)

-- Monoids

(++) :: Monoid a => a -> a -> a
(++) = mappend

-- IO

print :: (MonadIO m, Show s) => s -> m ()
print    = liftIO . Prelude.print

printLn :: MonadIO m => m ()
printLn = putStrLn ""

putStr :: MonadIO m => String -> m ()
putStr   = liftIO . Prelude.putStr

putStrLn :: MonadIO m => String -> m ()
putStrLn = liftIO . Prelude.putStrLn

prettyPrint :: (MonadIO m, Show s) => s -> m ()
prettyPrint = putStrLn . ppShow

--

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


isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _        = False

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _         = False


fromJustM :: Monad m => Maybe a -> m a
fromJustM Nothing  = fail "Prelude.fromJustM: Nothing"
fromJustM (Just x) = return x


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
whenRight' e f = whenRight e $ const f

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


lift2 :: (Monad (t1 m), Monad m,
          MonadTrans t, MonadTrans t1)
      => m a -> t (t1 m) a
lift2 = lift . lift


lift3 :: (Monad (t1 (t2 m)), Monad (t2 m), Monad m,
          MonadTrans t, MonadTrans t1, MonadTrans t2)
      => m a -> t (t1 (t2 m)) a
lift3 = lift . lift2


switch :: Monad m => m Bool -> a -> a -> m a
switch cond fail ok = do
  c <- cond
  return $ if c then ok else fail

mjoin :: Monoid a => a -> [a] -> a
mjoin delim l = mconcat (intersperse delim l)


show' :: (Show a, IsString s) => a -> s
show' = fromString . Prelude.show

foldlDef :: (a -> a -> a) -> a -> [a] -> a
foldlDef f d = \case
    []     -> d
    (x:xs) -> foldl f x xs


mapOver :: (Lens' a b) -> (b -> (b, out)) -> (a -> (a, out))
mapOver lens f s = (s & lens .~ a, out) where
    (a, out) = f $ s ^. lens

mapOverM :: Monad m => (Lens' a b) -> (b -> m (b, out)) -> (a -> m (a, out))
mapOverM lens f a = do
    (b, out) <- f $ a ^. lens
    return $ (a & lens .~ b, out)

--mapOver :: (Lens' b a) -> (a -> (out, a)) -> (b -> (out, b))
--mapOver lens f s = (out, s & lens .~ a) where
--    (out, a) = f $ s ^. lens

ifElseId :: Bool -> (a -> a) -> (a -> a)
ifElseId cond a = if cond then a else id