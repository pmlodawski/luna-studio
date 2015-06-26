

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE FunctionalDependencies            #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE DeriveDataTypeable            #-}
{-# LANGUAGE ScopedTypeVariables            #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE MagicHash            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE OverlappingInstances            #-}
{-# LANGUAGE ViewPatterns            #-}
{-# LANGUAGE UndecidableInstances            #-}
{-# LANGUAGE TypeFamilies            #-}

{-# LANGUAGE NoOverloadedStrings            #-}
{-# LANGUAGE TupleSections            #-}
{-# LANGUAGE StandaloneDeriving            #-}
{-# LANGUAGE DefaultSignatures            #-}
{-# LANGUAGE FlexibleInstances            #-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE PolyKinds #-}

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DatatypeContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Flowbox.System.Path where

--import           Control.Monad      ((>=>))
import           Data.Binary        (Binary)
import           Data.Typeable      hiding (cast)
--import           Data.List.Split    (splitOn)
--import qualified Data.List          as List
--import qualified Data.String.Utils  as StringUtils
import qualified System.Directory   as Directory
import qualified System.Environment as Env
import           Control.Exception  (catch, SomeException)
import           Control.Monad      (join)
--import           System.IO.Error    (IOError, isDoesNotExistError)
import           System.IO.Error    (tryIOError)
import           Data.Map           (Map)
import qualified Data.Map           as Map
--import           Control.Error.Safe (justErr)
--import           Data.Binary
import           Data.Monoid
import           Data.Maybe (fromJust)
import           Data.Foldable (fold)

--import           Flowbox.Prelude                    hiding (empty, fromList, toList)
--import qualified Flowbox.System.Directory.Locations as Directory
import System.IO.Unsafe (unsafePerformIO)
import Data.IORef
import Unsafe.Coerce (unsafeCoerce)
--import Flowbox.Prelude hiding (cons)
import GHC.TypeLits
import Data.Default

--import Control.Monad.Shuffle (deepBind, (>>>=), ($>>=))

--import Data.ConcreteTypeRep

import qualified Data.Map as Map
--import Control.Monad.Trans.Either
import Data.List (intercalate)
import GHC.Exts (Constraint)
import Control.Monad.State hiding (withState)
import Control.Applicative hiding (empty)

--------------------------

import           Data.List.Split    (splitOn)
import           GHC.Generics       (Generic)

import Control.Monad.Identity

import Flowbox.System.Variants hiding (main, cast)
import qualified Flowbox.System.Variants as Variants

import Control.Monad.Trans.Either (EitherT, runEitherT)
import qualified Control.Monad.Trans.Either as Either

#include "ghcplatform.h"


isEmpty = (== mempty)

---------------------------------

class MonadEither l m | m -> l where
    left :: l -> m a

instance Monad m => MonadEither l (EitherT l m) where
    left = Either.left

right :: (MonadEither l m, Monad m) => a -> m a
right = return


instance (MonadEither e m, Monad m) => MonadEither e (StateT s m) where
    left = lift . left

----------------------------------------------------


data Windows = Windows deriving (Show, Eq, Generic)
data Linux   = Linux   deriving (Show, Eq, Generic)
data Darwin  = Darwin  deriving (Show, Eq, Generic)

-- === utils ===

#ifdef darwin_HOST_OS
type Platform = Darwin
#endif

#ifdef linux_HOST_OS
type Platform = Linux
#endif

#ifdef mingw32_HOST_OS
type Platform = Windows
#endif

platform :: Platform
platform = def

-- === instances ===

instance Default Windows where def = Windows
instance Default Darwin  where def = Darwin
instance Default Linux   where def = Linux


-------------------------------------------------------------------------------------------------------------------

type Variants vars = (IsSet vars, WithVariants' VariantRawShow (RecordTemplate vars) String)
------------------------------------------------------------------------
-- Path
------------------------------------------------------------------------

data Error = UnknownSpecial String
           | UnknownVar     String
           | NegativePath
           deriving (Show)


data Segment segs = Root
                  | Node String
                  | Segment (Record segs)

deriving instance Eq (Record segs) => Eq (Segment segs)

--deriving instance Variants segs => Show (Segment segs)
instance Variants segs => Show (Segment segs) where
    showsPrec d = \case
        Root               -> showString "Root"
        Node    s          -> showParen (d > app_prec) $ showString "Node " . showsPrec (app_prec+1) s
        Segment (Record r) -> showParen (d > app_prec) $ showString $ "Segment (" <> variantShow r <> ")"
        where app_prec = 10

newtype Path (base :: [*]) (segs :: [*]) = Path { fromPath :: [Segment segs] } deriving (Typeable, Monoid, Default)

--instance (Functor m, Monad m, IsSet segs, WithVariantsM tp (Record segs) m (Record segs'))
--      => WithVariantsM tp (Path base segs) m (Path base segs') where
--    withVariantsM tp = withSegmentsM (withVariantsM tp)

--instance (Monad m, WithVariantsM tp a m a') => WithVariantsM tp [a] m [a'] where
--    withVariantsM = mapM . withVariantsM

--instance (Monad m, Functor m, IsSet segs, WithVariantsM tp (Record segs) m (Record segs'))
--      => WithVariantsM tp (Segment segs) m (Segment segs') where
--    withVariantsM tp = \case
--        Root      -> return Root
--        Node    s -> return $ Node s
--        Segment s -> Segment <$> withVariantsM tp s

withSegments :: ([Segment segs] -> [Segment segs']) -> Path base segs -> Path base segs'
withSegments f = Path . f . fromPath

withSegmentsM :: Functor m => ([Segment segs] -> m [Segment segs']) -> Path base segs -> m (Path base segs')
withSegmentsM f = fmap Path . f . fromPath

deriving instance Eq (Segment segs) => Eq (Path base segs)
deriving instance Variants segs => Show (Path base segs)

---




type RelPath = Path '[Var]


nonEmpty = filter (/= mempty)


readPath :: RecordReader segs => String -> Either Error (Path base segs)
readPath str@(s:_)  = fmap (Path . mount . concat) . sequence $ fmap (Right  . readPathSegment) segs where
    mount     = if s == '/' then (Root:) else id
    segs      = nonEmpty $ splitOn "/" str
    --readSeg s = if and $ fmap (=='.') s
--        then Right $ replicate (length s - 1) Up
--        else fmap return $ case s of
--            --"~"    -> Right home
--            (a:as) -> case a of
--                '$' -> Right $ Var as
--                --'@' -> readSpecial as
--                _   -> Right . Segment $ Node s

--class SegmentsReader s where
--    readSegments :: String -> Maybe (Record s)

--instance SegmentsReader segs => SegmentsReader (seg ': segs) where
--    readSegments s = case (readSegment s :: Maybe seg) of
--        Just x -> Just $ cons x
--        _      -> readSegments

--type VariantCons nsyIdx (Index (NatRep t) (VariantReprsOf r)) t r

---

data VariantSegmentShow = VariantSegmentShow
type SegmentShowCtx a   = WithVariants VariantSegmentShow (RecordTemplate a) String

instance (PathSegment a, Monad m) => WithVariantM VariantSegmentShow a m String where
    withVariantM _ = return . showSegment

---


showPath :: (IsSet segs, SegmentShowCtx segs) => Path base segs -> String
showPath (Path segs) = intercalate "/" $ fmap showSeg segs where
    showSeg = \case
        Root      -> "/"
        Node    s -> s
        Segment s -> withVariants VariantSegmentShow s

type RecordReader s = RecordReader' s s

class RecordReader' (s :: [*]) rs where
    readRecord :: Proxy s -> String -> Maybe [Record rs]

instance (VariantCons r seg, PathSegment seg, RecordReader' segs r) => RecordReader' (seg ': segs) r where
    readRecord _ s = case (readSegment s :: Maybe [seg]) of
        Just x  -> Just $ fmap cons x
        Nothing -> readRecord (Proxy :: Proxy segs) s

instance RecordReader' '[] r where
    readRecord _ _ = Nothing

class PathSegment s where
    readSegment :: String -> Maybe [s]
    showSegment :: s -> String



readPathSegment :: forall (segs :: [*]). RecordReader segs => String -> [Segment segs]
readPathSegment s = case readRecord (Proxy :: Proxy segs) s of
    Just s  -> fmap Segment s
    Nothing -> [Node s]


---

data Up = Up deriving (Show, Eq, Typeable)
type instance NatRep Up = '[0]

instance PathSegment Up where
    readSegment s = if and $ fmap (=='.') s
        then Just $ replicate (length s - 1) Up
        else Nothing
    showSegment _ = ".."

instance SegmentExpander sys Up m segs where
    expandSegment _ p = do
        expPath <- get
        when (isEmpty expPath) $ left NegativePath
        put $ withSegments init expPath
        return mempty

---

data Var = Var String deriving (Show, Eq, Typeable)
type instance NatRep Var = '[1]

instance PathSegment Var where
    readSegment = \case
        ('$':s) -> Just [Var s]
        _       -> Nothing
    showSegment (Var s) = '$' : s


--instance SegmentExpander sys Var m segs where
--    expandSegment _ (Var v) = do
--        envVar <- Env.getEnv v
--        envPath <-
--        when (isEmpty expPath) $ left NegativePath
--        put $ withSegments init expPath
--        return mempty

                    --Var v -> do
                    --    envVal  <- hoistEither $ justErr (UnknownVar v) (Map.lookup v env)
                    --    envPath <- hoistEither $ readRelPath envVal
                    --    expPath <- expandPath' envPath
                    --    expandSegments (expPath // path) ssPath


---

data Temp = Temp Locality Persistence
             deriving (Show, Eq, Typeable)

data Locality = Global
              | Local
              deriving (Show, Eq, Generic)

data Persistence = Persistent
                 | NonPersistent
                 deriving (Show, Eq, Typeable)


type instance NatRep Temp = '[2]

instance PathSegment Temp where
    readSegment = readSpecial "temp" [Temp Local NonPersistent]
    showSegment _ = showSpecial "temp"

readSpecial s val = \case
    ('@' : str) -> if s == str then Just val
                               else Nothing
    _           -> Nothing

showSpecial = ('@':)

---


type ExpandCtx base p m p' = ( Monad m, Functor m, IsSet p
                         , WithVariantsM (Expand (Diff p p')) (Record p) (StateT (Path base p') (EitherT Error m)) (Path base p')
                         )


data Expand (segs :: [*]) = Expand

instance (selected ~ In a segs, SegmentCondExpander selected a m (Path base r), Functor m)
      => WithVariantM (Expand segs) a m (Path base r) where
    withVariantM _ = expandSegmentCond (Proxy :: Proxy selected)

class SegmentCondExpander (cond :: Bool) a m r where
    expandSegmentCond :: Proxy cond -> a -> m r

instance (VariantCons r a, Monad m) => SegmentCondExpander False a m (Path base r) where
    expandSegmentCond _ s = return $ Path [Segment $ cons s]

instance (SegmentExpander Platform a m r, MonadState (Path base r) m, MonadEither Error m, Functor m)
      => SegmentCondExpander True a m (Path base r) where
    expandSegmentCond _ = expandSegment platform




class SegmentExpander sys s m segs where
    expandSegment :: (Monad m, Functor m, MonadState (Path base segs) m, MonadEither Error m)
                  => sys -> s -> m (Path base segs)

data ExpandState base segs = ExpandState { _expanded :: Path base segs }

deriving instance Variants segs => Show (ExpandState base segs)

-- todo: ponizej segs zmienic na pusta liste!
instance Default (ExpandState base segs) where
    def = ExpandState def

expandPath :: forall p m p' base. ExpandCtx base p m p' => Path base p -> m (Either Error (Path base p'))
expandPath (Path segs) = runEitherT
                       $ flip execStateT (def :: Path base p')
                       $ mapM_ expandPathSegment segs where
    expandNewSegments = \case
        Root      -> return $ Path [Root]
        Node    s -> return $ Path [Node s]
        Segment s -> withVariantsM mkExpand s

    expandPathSegment p = do
        expPath <- expandNewSegments p
        Path pathBuilder <- get
        put . Path $ pathBuilder <> fromPath expPath
        return (expPath :: Path base p')

    mkExpand = Expand :: Expand (Diff p p')


--class PathJoin a b | a b -> c where
--    pathJoin :: Path base a -> Path base b -> Path base c

--pathJoin :: Path base a -> Path base b -> Path base (Union a b)
--pathJoin p p' =



--type ExpandSingle v = (p' ~ Remove v p, ExpandCtx base p m p') => Path base p -> m (Either Error (Path base p'))

--expandUp :: ExpandSingle Up
--expandUp = expandPath

--expandVar :: ExpandSingle Var
--expandVar = expandPath


---

--instance

class Castx p p' where
    cast :: p -> p'


--instance Cast (Path base p) (Path base p') where
--    cast = \case

instance (IsSet p, Cast (Record p) (Record p')) => Castx (Segment p) (Segment p') where
    cast = \case
        Root -> Root
        Node s -> Node s
        Segment s -> Segment $ Variants.cast s

deriving instance Typeable '[]
deriving instance Typeable '(:)

main = do


    let --p = var "foo" :: Segment '[Var]
        p2 = readPath "@temp/bar/$foo/.." :: (Either Error (Path '[Up, Var, Temp] '[Up, Var, Temp]))
        p3 = readPath "bar/$foo/.." :: (Either Error (Path '[Up, Var, Temp] '[Up, Var]))

    --print p
    print p2

    case p2 of
        Left _  -> return ()
        Right p -> do
            print $ showPath p
            --(print . fmap showPath) =<< expandUp p
            --(print . fmap showPath) =<< expandVar p
            --(print . showPath) $ runIdentity $ expandUp p
            return ()

    case p3 of
        Left _  -> return ()
        Right p -> do
            --let px = cast p :: Maybe (Path '[Up, Var, Temp] '[Up, Var, Temp])
            print $ showPath p
            --print $ px
            --(print . fmap showPath) =<< expandUp p
            --(print . fmap showPath) =<< expandVar p
            --(print . showPath) $ runIdentity $ expandUp p
            return ()

    --let f = Foo (R1_V1 (1::Int)) :: Foo '[Int]

    print "end"

-- TODO - DOKONCZYC STATE


